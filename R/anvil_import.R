#' Import data tables into AnVIL
#' 
#' Import data tables into AnVIL
#' 
#' @name anvil_import
#' @param table Data table to import (tibble or data.frame)
#' @param table_name Name of data table in model
#' @param model \code{\link{dm}} object describing data model
#' 
#' @import AnVIL
#' @export
anvil_import_table <- function(table, table_name, model) {
    # does table already exist?
    # - do the columns match?
    # - are there any overlapping samples?
    
    table <- add_entity_id(table, table_name, model)
    avtable_import(table, entity=paste0(table_name, "_id"))
}


anvil_get_table_columns <- function() {
    tables <- avtables()
    strsplit(tables$colnames, split=", ", fixed=TRUE)
}


add_entity_id <- function(table, table_name, model) {
    pk <- dm_get_all_pks(model, table=table_name)$pk_col
    if (length(pk) == 0) stop("no primary key defined")
    if (length(pk) > 1) stop("invalid model; >1 primary key")
    pk <- pk[[1]]
    
    # is this a compound key?
    if (length(pk) > 1) {
        entity <- do.call(paste, c(lapply(pk, function(x) table[[x]]), sep="_"))
        table[[paste0(table_name, "_id")]] <- entity
    # do we need to create a column matching the table name?
    } else if (sub("_id$", "", pk) != table_name)  {
        table[[paste0(table_name, "_id")]] <- table[[pk]]
    }
    
    return(table)
}
