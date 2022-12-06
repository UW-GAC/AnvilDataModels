#' Import data from json file
#' 
#' Import data model in json format
#' 
#' \code{json_to_dm} returns a \code{\link{dm}} object.
#' 
#' \code{json_to_dbml} writes a \href{https://www.dbml.org}{DBML} file.
#' 
#' json files to be imported should contain a list "tables". Each table object should
#' contain the following elements: 
#' \itemize{
#'   \item{table: }{table name}
#'   \item{columns: }{list of "column" objects}
#'   \item{required: }{TRUE indicates the table is required, FALSE or missing if the table
#'    is optional. 'CONDITIONAL (t1)' indicates that a table is only required if table 't1' is 
#'    present.}
#' }
#' 
#' Each column object should contain the following elements:
#' \itemize{
#'   \item{column: }{column namee}
#'   \item{data_type: }{"string", "boolean, "integer", "float", "date", "dateTime", or "enumeration"}
#'   \item{required: }{TRUE indicates the column is required, FALSE or missing if the column
#'    is optional. 'CONDITIONAL (column = value)' indicates a requirement only if any element of 
#'    'column' contains 'value'.}
#'   \item{primary_key: }{logical where TRUE indicates the column is a primary key, 
#'     other values may be FALSE or missing}
#'   \item{references: }{Reference to other columns in the data model. Either 
#'     1) a cross-table reference string following the 
#'     \href{https://www.dbml.org/docs/#relationships-foreign-key-definitions}{DBML}
#'     format, or 
#'     2) 'from:' followed by a comma-separated list of columns used to
#'     automatically generate this column using a hash function.}
#'   \item{description}{Note string (column description) following the
#'     \href{https://www.dbml.org/docs/#column-notes}{DBML} format}
#' }
#' 
#' @name import_json
#' @param json Character vector with paths to json files. If more than one file is provided, they will be combined into a single data model.
#' @return \code{\link{dm}} object
#' 
#' @examples 
#' json <- system.file("extdata", "data_model.json", package="AnvilDataModels")
#' (dm <- json_to_dm(json))
#' attr(dm, "required")
#' lapply(dm, attr, "required")
#' 
#' tmp <- tempfile()
#' json_to_dbml(json, tmp)
#' readLines(tmp, n=14)
#' 
#' unlink(tmp)
#' 
#' json <- system.file("extdata", "data_model_conditional.json", package="AnvilDataModels")
#' (dm <- json_to_dm(json))
#' attr(dm, "conditions")
#' lapply(dm, attr, "conditions")
#' 
#' @import dm
#' @importFrom dplyr as_tibble %>%
#' @importFrom lubridate ymd ymd_hms
#' @importFrom stats setNames
#' @export
json_to_dm <- function(json) {
    dat <- .read_data_model(json)
    
    # unique tables
    tables <- dat$tables
    if (length(tables) == 0) stop("Data model must contain Table entities")
    
    # map database types to R types
    type_map <- list("string"=character(),
                     "boolean"=logical(),
                     "integer"=integer(),
                     "float"=numeric(),
                     "date"=ymd(),
                     "dateTime"=ymd_hms())
    
    # create 0-row tibbles for each table
    table_list <- lapply(tables, function(t) {
        tab <- lapply(t$columns, function(c) {
            type <- c$data_type
            if (type %in% names(type_map)) {
                return(type_map[[type]])
            } else if (type == "enumeration") {
                return(factor(levels=c$enumerations))
            } else {
                stop("Undefined data type")
            }
        })
        names(tab) <- sapply(t$columns, function(c) c$column)
        tib <- as_tibble(tab)
        req <- .parse_requirements(t, type="column")
        attr(tib, "required") <- req$required
        attr(tib, "conditions") <- req$conditions
        return(tib)
    })
    names(table_list) <- sapply(tables, function(t) t$table)
    
    # coerce list of tables to dm object
    data_model <- as_dm(table_list)
    
    # add primary keys
    for (t in tables) {
        pk <- .named_elements(t$columns, "column", "primary_key")
        if (length(pk) > 0) {
            data_model <- dm_add_pk(data_model, table=!!t$table, columns=!!names(pk))
        }
    }
    
    # add foreign keys
    for (t in tables) {
        fk <- .named_elements(t$columns, "column", "references")
        fk <- fk[.valid_ref(fk)]
        for (i in seq_along(fk)) {
            ref <- strsplit(fk[i], " ")[[1]][[2]] %>%
                strsplit(".", fixed=TRUE) %>%
                unlist()
            data_model <- dm_add_fk(data_model, table=!!t$table, columns=!!names(fk)[i],
                                    ref_table=!!ref[1], ref_columns=!!ref[2])
        }
    }
    
    # set which tables are required
    req <- .parse_requirements(tables, type="table")
    attr(data_model, "required") <- req$required
    attr(data_model, "conditions") <- req$conditions
    
    # set which columns are generated from other columns
    auto_id <- list()
    for (t in tables) {
        auto <- .named_elements(t$columns, "column", "references")
        auto <- auto[.valid_from(auto)]
        if (length(auto) > 0) {
            auto_id[[t$table]] <- lapply(auto, .column_from)
        }
    }
    attr(data_model, "auto_id") <- auto_id
    
    # add version
    attr(data_model, "version") <- dat$version
    
    return(data_model)
}



#' @rdname import_json
#' @param dbml Path for the DBML output file
#' @importFrom stats na.omit
#' @export
json_to_dbml <- function(json, dbml) {
    dat <- .read_data_model(json)
    
    # output file stream
    con <- file(dbml, "w")
    
    # version
    writeLines(paste("// version", dat$version), con)
    
    # tables
    tables <- dat$tables
    for (t in tables) {
        writeLines(paste("Table", t$table, "{"), con)
        
        for (c in t$columns) {
            pk <- if (!is.null(c$primary_key) && c$primary_key) "pk" else NA
            ref <- if (!is.null(c$references) && .valid_ref(c$references)) paste("ref:", c$references) else NA
            esc_quotes <- paste0("note: '", gsub("'", "\\'", c$description, fixed=TRUE), "'")
            note <- if (!is.null(c$description)) esc_quotes else NA
            meta <- paste(na.omit(c(pk, ref, note)), collapse=", ")
            if (nchar(meta) > 0 ) meta <- paste0("[", meta, "]")
            writeLines(paste(" ", c$column, c$data_type, meta), con)
        }
        
        # do we have a composite primary key?
        pk <- .named_elements(t$columns, "column", "primary_key")
        if (length(pk) > 1) {
            writeLines("  indexes {", con)
            index <- paste0(names(pk)[pk], collapse=", ")
            writeLines(paste0("    (", index, ") [pk]"), con)
            writeLines("  }\n", con)
        }

        writeLines("}\n", con)
    }
    
    close(con)
}


# read json file(s) to list
#' @importFrom jsonlite fromJSON
.read_data_model <- function(json) {
    if (length(json) == 1) {
        return(fromJSON(json, simplifyDataFrame=FALSE))
    } else {
        dat_list <- lapply(json, fromJSON, simplifyDataFrame=FALSE)
        dat <- dat_list[[1]]
        dat$tables <- c(dat$tables, dat_list[[2]]$tables)
        return(dat)
    }
}


# returns named vector of elements in list
.named_elements <- function(x, name, element) {
    setNames(lapply(x, function(y) y[[element]]),
             lapply(x, function(y) y[[name]])) %>%
        unlist()
}


# parses required column to separate logical from conditional
#' @importFrom stringr str_extract_all
.parse_requirements <- function(x, type=c("column", "table")) {
    type <- match.arg(type)
    if (type == "column") {
        col <- .named_elements(x$columns, "column", "required")
    } else {
        col <- .named_elements(x, "table", "required")
    }
    ind <- grepl("^CONDITIONAL", col)
    req <- col[!ind]
    mode(req) <- "logical"
    req <- names(req)[req]
    cond <- col[ind]
    if (length(cond) > 0) {
        cond <- setNames(unlist(str_extract_all(cond, "(?<=\\().+?(?=\\))")), names(cond))
    }
    return(list(required=req, conditions=cond))
}


# returns logical vector for whether a cross-table reference is valid
.valid_ref <- function(x) {
    !(is.na(x)) & !grepl("^from:", x)
}


# returns logical vector for whether an auto-id reference is valid
.valid_from <- function(x) {
    grepl("^from:", x)
}


# parses 'from:' reference to return list of columns
#' @importFrom stringr str_trim
.column_from <- function(x) {
    sub("^from:", "", x) %>%
        strsplit(",", fixed=TRUE) %>%
        unlist() %>%
        str_trim()
}


#' transpose field,value pairs to a data model
#' @param fv data frame with columns field, value
#' @param table_name name of table in model
#' @param model \code{\link{dm}} object describing data model
#' @return single row data frame with names from \code{fv$field} and values from \code{fv$value}
#' @importFrom dplyr bind_cols
#' @importFrom methods as
#' @export
transpose_field_value <- function(fv, table_name, model) {
    stopifnot(setequal(names(fv), c("field", "value")))
    mod <- model[[table_name]]
    lapply(setNames(1:nrow(fv), fv$field), function(i) {
        f <- fv$field[i]
        v <- fv$value[i]
        # if field is not in model, can't do anything with it
        if (!(f %in% names(mod))) { 
            return(v)
        }
        if (is.factor(mod[[f]])) {
            x <- factor(v, levels=levels(mod[[f]]))
        } else if (is.Date(mod[[f]])) {
            x <- ymd(v)
        } else {
            x <- as(v, class(mod[[f]]))
        }
        return(x)
    }) %>%
        bind_cols()
}
