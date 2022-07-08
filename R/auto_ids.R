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
#' @return \code{table} with additional columns
#' @export
add_auto_columns <- function(table, table_name, model, nchar=8) {
    auto_ids <- attr(model, "auto_id")[[table_name]]
    for (col in names(auto_ids)) {
        table[[col]] <- apply(table[,auto_ids[[col]]], 1, paste, collapse="") %>%
            hash_id(nchar=nchar)
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
