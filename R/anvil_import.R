#' Import data tables into AnVIL
#' 
#' Import data tables into AnVIL
#' 
#' @name anvil_import
#' @param table Data table to import (tibble or data.frame)
#' @param table_name Name of data table in model
#' @param model \code{\link{dm}} object describing data model
#' @param overwrite Logical for whether to overwrite data for existing rows
#' 
#' @import AnVIL
#' @export
anvil_import_table <- function(table, table_name, model, overwrite=FALSE) {
    # add entity id first, so we can compare to anvil
    table <- add_entity_id(table, table_name, model)
  
    # does table already exist?
    anvil_tables <- avtables()
    if (table_name %in% anvil_tables$table) {
        message("Table ", table_name, " already exists")
        anvil_table <- avtable(table_name)
        
        # do the columns match?
        if (!setequal(names(table), names(anvil_table))) {
            stop("Columns of new table do not match existing table\n",
                 "  AnVIL: ", paste(names(anvil_table), collapse=", "), "\n",
                 "  table: ", paste(names(table), collapse=", "))
        }
        
        # are there any overlapping entities?
        # from AnVIL docs: Terra will only overwrite data rows with the same ID 
        # (in the first column). If the TSV (load) file includes different IDs, 
        # these rows will be added to the existing table.
        entity_id <- paste0(table_name, "_id")
        anvil_entity <- anvil_table[[entity_id]]
        this_entity <- table[[entity_id]]
        overlaps <- length(intersect(this_entity, anvil_entity))
        if (overlaps > 0) {
            if (overwrite) {
              message("  Overwriting ", overlaps, " rows")
            } else {
                stop("Some entities in table '", table_name, "' already exist\n",
                     "  Set overwrite=TRUE to overwrite these rows")
            }
        }
    }
    
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
