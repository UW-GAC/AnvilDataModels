#' Check tables against data model
#' 
#' Read a set of files containing data tables and check them against a data model.
#' 
#' @name check_data_tables
#' @param files Vector of file paths, one per data table.
#' @param table_names Vector of table names associated with \code{files}.
#' @param quiet Logical to control printing results of column parsing from \code{\link{read_tsv}}.
#' @return \code{read_data_tables} returns a named list of data frames.
#' 
#' @examples
#' # read data model
#' tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
#' model <- tsv_to_dm(tsv)
#' 
#' # read tables to check
#' table_names <- c("subject", "phenotype", "sample", "file")
#' files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
#' names(files) <- table_names
#' tables <- read_data_tables(files)
#' 
#' check_table_names(tables, model)
#' check_column_names(tables, model)
#' check_column_types(tables, model)
#' check_primary_keys(tables, model)
#' check_foreign_keys(tables, model)
#' 
#' @importFrom readr read_tsv
#' @export
read_data_tables <- function(files, table_names=names(files), quiet=TRUE) {
    names(files) <- table_names
    lapply(files, read_tsv, show_col_types=!quiet)
}


#' @rdname check_data_tables
#' @param tables Named list of data tables
#' @param model \code{\link{dm}} object describing data model
#' @return \code{check_table_names} returns \code{NULL} if \code{tables} matches \code{model}, 
#'     or a list:
#'     \itemize{
#'         \item{missing_tables: }{Vector of tables in \code{model} but not in \code{tables}}
#'         \item{extra_tables: }{Vector of tables in \code{tables} but not in \code{model}}
#'     }
#'     
#' @export
check_table_names <- function(tables, model) {
    if (setequal(names(tables), names(model))) {
        return(NULL)
    } else {
        return(list(missing_tables=setdiff(names(model), names(tables)),
                    extra_tables=setdiff(names(tables), names(model))))
    }
}


#' @rdname check_data_tables
#' @return \code{check_column_names} return a list of all tables in common between data 
#'     and model. Each table element is \code{NULL} if columns in \code{tables} matches \code{model}, 
#'     or a list:
#'     \itemize{
#'         \item{missing_columns: }{Vector of columns in \code{model} but not in \code{tables}}
#'         \item{extra_columns: }{Vector of columns in \code{tables} but not in \code{model}}
#'     }
#'     
#' @export
check_column_names <- function(tables, model) {
    common <- intersect(names(tables), names(model))
    chk <- lapply(common, function(t) {
        if (setequal(names(tables[[t]]), names(model[[t]]))) {
            return(NULL)
        } else {
            return(list(missing_columns=setdiff(names(model[[t]]), names(tables[[t]])),
                        extra_columns=setdiff(names(tables[[t]]), names(model[[t]]))))
        }
    })
    names(chk) <- common
    return(chk)
}


.try_conversion <- function(x, name, type, FUN) {
    err_string <- paste("Some values of", name, "not compatible with", type, "type")
    tryCatch({
        if (all(FUN(x) == x)) {
            return(NULL)
        } else {
            return(err_string)
        }
    }, warning=function(w) err_string, error=function(e) err_string)    
}

#' @rdname check_data_tables
#' @return \code{check_column_types} returns a list of all tables in common between data 
#'     and model. Each table element is a list of all columns in common between table and 
#'     model. Each column element is \code{NULL} if values in column are a compatible type 
#'     with the data model, or a string describing the mismatch.
#'     
#' @importFrom lubridate is.Date is.timepoint ymd ymd_hms
#' @export
check_column_types <- function(tables, model) {
    common <- intersect(names(tables), names(model))
    chk <- lapply(common, function(t) {
        cols <- intersect(names(tables[[t]]), names(model[[t]]))
        chk2 <- lapply(cols, function(c) {
            ct <- tables[[t]][[c]]
            cm <- model[[t]][[c]]
            if (is.character(cm)) {
                .try_conversion(ct, name=paste(t, c, sep="."), type="character", FUN=as.character)
            } else if (is.logical(cm)) {
                .try_conversion(ct, name=paste(t, c, sep="."), type="boolean", FUN=as.logical)
            } else if (is.integer(cm)) {
                .try_conversion(ct, name=paste(t, c, sep="."), type="integer", FUN=as.integer)
            } else if (is.numeric(cm)) {
                .try_conversion(ct, name=paste(t, c, sep="."), type="float", FUN=as.numeric)
            } else if (is.Date(cm)) {
                .try_conversion(ct, name=paste(t, c, sep="."), type="date", FUN=ymd)
            } else if (is.timepoint(cm)) {
                .try_conversion(ct, name=paste(t, c, sep="."), type="datetime", FUN=ymd_hms)
            } else if (is.factor(cm)) {
                .try_conversion(ct, name=paste(t, c, sep="."), type="enum", 
                                FUN=function(x) factor(x, levels=levels(cm)))
            } else {
                stop("unrecognized data type in model for ", t, ".", c)
            }
        })
        names(chk2) <- cols
        return(chk2)
    })
    names(chk) <- common
    return(chk)
}


#' @rdname check_data_tables
#' @return \code{check_primary_keys} returns the results of \code{\link{dm_examine_constraints}}
#'     after applying primary keys from \code{model} to \code{tables}.
#' @export
check_primary_keys <- function(tables, model) {
    keys <- dm_get_all_pks(model)
    tables_dm <- as_dm(tables)
    for (i in 1:nrow(keys)) {
        tables_dm <- dm_add_pk(tables_dm, table=!!keys$table[i], columns=!!keys$pk_col[[i]])
    }
    dm_examine_constraints(tables_dm)
}


#' @rdname check_data_tables
#' @return \code{check_foreign_keys} returns the results of \code{\link{dm_examine_constraints}}
#'     after applying foreign keys from \code{model} to \code{tables}.
#' @export
check_foreign_keys <- function(tables, model) {
    keys <- dm_get_all_fks(model)
    tables_dm <- as_dm(tables)
    for (i in 1:nrow(keys)) {
        tables_dm <- dm_add_fk(tables_dm, 
                               table=!!keys$child_table[i], 
                               columns=!!keys$child_fk_cols[[i]],
                               ref_table=!!keys$parent_table[i],
                               ref_columns=!!keys$parent_key_cols[[i]])
    }
    dm_examine_constraints(tables_dm)
}
