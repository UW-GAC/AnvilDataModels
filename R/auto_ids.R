#' Add columns with auto-generated ids
#' 
#' Add columns to a table using a hash of other columns, as defined in a data model
#' 
#' Automatically generated hashed identifiers based on the values of
#' other columns in \code{table}. \code{model} should have an attribute
#' \code{auto_id} containing a vector of columns to be used.
#' 
#' @param table Data table to import (tibble or data.frame)
#' @param table_name Name of data table in model
#' @param model \code{\link{dm}} object describing data model
#' @param nchar number of characters in the resulting strings (max 32)
#' @param error_on_missing Logical for whether to throw an error if any columns necessary to create another column are missing. If FALSE, the function will issue a warning but proceed without adding the column.
#' @return \code{table} with additional columns
#' @export
add_auto_columns <- function(table, table_name, model, nchar=8, error_on_missing=TRUE) {
    auto_ids <- attr(model, "auto_id")[[table_name]]
    for (col in names(auto_ids)) {
        missing_ids <- setdiff(auto_ids[[col]], names(table))
        if (length(missing_ids) == 0) {
            table[[col]] <- apply(table[,auto_ids[[col]]], 1, paste, collapse="") %>%
                hash_id(nchar=nchar)
        } else if (length(missing_ids) > 0 & error_on_missing) {
            stop("Cannot create id ", col, "; missing required columns ", paste(missing_ids, collapse=", "))
        } else if (length(missing_ids) > 0 & !error_on_missing) {
            warning("Cannot create id ", col, "; missing required columns ", paste(missing_ids, collapse=", "))
        }
    }
    table
}


#' create a hashed identifier
#' @rdname add_auto_columns
#' @param x string use to create hash
#' @return identifier based on a hash of \code{x}
#' @importFrom openssl md5
#' @export
hash_id <- function(x, nchar=8) {
    substr(md5(x), 1, nchar)
}
