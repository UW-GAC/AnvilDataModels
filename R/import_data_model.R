#' Import data from tsv file
#' 
#' Import data model in tsv format
#' 
#' \code{tsv_to_dm} returns a \code{\link{dm}} object.
#' 
#' \code{tsv_to_dbml} writes a \href{https://www.dbml.org}{DBML} file.
#' 
#' TSV files to be imported must have the following columns: 
#' \itemize{
#'   \item{entity: }{"Table" or "enum"}
#'   \item{table: }{table or enum name}
#'   \item{column: }{column name within table}
#'   \item{type: }{"string", "boolean, "integer", "float", "date", "dateTime", or name of enum}
#'   \item{required: }{logical where TRUE indicates the column is required}
#'   \item{pk: }{logical where TRUE indicates the column is a primary key, 
#'     other values may be FALSE or missing}
#'   \item{ref: }{Reference string following the 
#'     \href{https://www.dbml.org/docs/#relationships-foreign-key-definitions}{DBML}
#'     format: }
#'   \item{note}{Note string (column description) following the
#'     \href{https://www.dbml.org/docs/#column-notes}{DBML} format}
#' }
#' 
#' @name import_tsv
#' @param tsv Character vector with paths to tab-separated variable files. If more than one file is provided, they will be combined into a single data model.
#' @return \code{\link{dm}} object
#' 
#' @examples 
#' tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
#' (dm <- tsv_to_dm(tsv))
#' 
#' tmp <- tempfile()
#' tsv_to_dbml(tsv, tmp)
#' readLines(tmp, n=14)
#' 
#' unlink(tmp)
#' 
#' @import dm
#' @importFrom dplyr as_tibble filter .data %>%
#' @importFrom lubridate ymd ymd_hms
#' @export
tsv_to_dm <- function(tsv) {
    dat <- .read_data_model(tsv)
    
    # unique tables and enums
    tables <- unique(filter(dat, .data[["entity"]] == "Table")$table)
    enums <- unique(filter(dat, .data[["entity"]] == "enum")$table)
    
    # transform enums into factors
    enum_list <- lapply(enums, function(e) {
        this <- filter(dat, .data[["entity"]] == "enum" & .data[["table"]] == e)
        return(factor(levels=this$column))
    })
    names(enum_list) <- enums
    
    # map database types to R types
    type_map <- list("string"=character(),
                    "boolean"=logical(),
                    "integer"=integer(),
                    "float"=numeric(),
                    "date"=ymd(),
                    "dateTime"=ymd_hms())
    
    # create 0-row tibbles for each table
    table_list <- lapply(tables, function(t) {
        this <- filter(dat, .data[["entity"]] == "Table" & .data[["table"]] == t)
        tab <- lapply(this[["type"]], function(y) {
            if (y %in% names(type_map)) {
                return(type_map[[y]])
            } else if (y %in% enums) {
                return(enum_list[[y]])
            } else {
                stop("undefined type")
            }
        })
        names(tab) <- this$column
        tib <- as_tibble(tab)
        attr(tib, "required") <- ifelse(is.na(this$required), FALSE, this$required)
        return(tib)
    })
    names(table_list) <- tables
    
    # coerce list of tables to dm object
    data_model <- as_dm(table_list)
    
    # add primary keys
    pk <- filter(dat, .data[["pk"]])
    tables_pk <- unique(pk$table)
    for (t in tables_pk) {
        this <- filter(pk, .data[["entity"]] == "Table" & .data[["table"]] == t)
        data_model <- dm_add_pk(data_model, table=!!t, columns=!!this$column)
    }
    
    # add foreign keys
    fk <- filter(dat, !(is.na(.data[["ref"]])))
    for (i in 1:nrow(fk)) {
        ref <- strsplit(fk$ref[i], " ")[[1]][[2]] %>%
            strsplit(".", fixed=TRUE) %>%
            unlist()
        data_model <- dm_add_fk(data_model, table=!!fk$table[i], columns=!!fk$column[i],
                                ref_table=!!ref[1], ref_columns=!!ref[2])
    }
    
    return(data_model)
}


#' @rdname import_tsv
#' @param dbml Path for the DBML output file
#' @importFrom stats na.omit
#' @export
tsv_to_dbml <- function(tsv, dbml) {
    dat <- .read_data_model(tsv)
    
    # output file stream
    con <- file(dbml, "w")
    
    # tables
    tables <- unique(filter(dat, .data[["entity"]] == "Table")$table)
    for (t in tables) {
        writeLines(paste("Table", t, "{"), con)
        
        this <- filter(dat, .data[["entity"]] == "Table" & .data[["table"]] == t)
        for (i in 1:nrow(this)) {
            pk <- if (!is.na(this$pk[i]) && this$pk[i]) "pk" else NA
            ref <- if (!is.na(this$ref[i])) paste("ref:", this$ref[i]) else NA
            esc_quotes <- paste0("note: '", gsub("'", "\\'", this$note[i], fixed=TRUE), "'")
            note <- if (!is.na(this$note[i])) esc_quotes else NA
            meta <- paste(na.omit(c(pk, ref, note)), collapse=", ")
            if (nchar(meta) > 0 ) meta <- paste0("[", meta, "]")
            writeLines(paste(" ", this$column[i], this$type[i], meta), con)
        }
        
        # do we have a composite primary key?
        if (sum(this$pk, na.rm=TRUE) > 1) {
            writeLines("  indexes {", con)
            index <- paste0(na.omit(this$column[this$pk]), collapse=", ")
            writeLines(paste0("    (", index, ") [pk]"), con)
            writeLines("  }\n", con)
        }

        writeLines("}\n", con)
    }
    
    # enums
    enums <- unique(filter(dat, .data[["entity"]] == "enum")$table)
    for (e in enums) {
        writeLines(paste("enum", e, "{"), con)
        
        this <- filter(dat, .data[["entity"]] == "enum" & .data[["table"]] == e)
        for (i in 1:nrow(this)) {
            writeLines(paste(" ", this$column[i]), con)
        }
        
        writeLines("}\n", con)
    }
    
    close(con)
}


# read tsv file(s) to tibble
#' @importFrom readr read_tsv
#' @importFrom dplyr bind_rows
.read_data_model <- function(tsv) {
    cols <- c("entity", "table", "column", "type", "required", "pk", "ref", "note")
    dat <- bind_rows(lapply(tsv, read_tsv, col_names=TRUE, col_types=paste(rep("c", length(cols)), collapse="")))
    stopifnot(setequal(names(dat), cols))
    dat$required <- as.logical(dat$required)
    dat$pk <- as.logical(dat$pk)
    return(dat)
}
