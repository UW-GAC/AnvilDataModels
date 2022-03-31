#' Import data tables into AnVIL
#' 
#' Import data tables into AnVIL
#' 
#' \code{anvil_import_table} imports a data table to an AnVIL workspace.
#' 
#' \code{anvil_import_set} imports a set table to an AnVIL workspace. The
#' reference table must already exist in the workspace.
#' 
#' \code{create_set_all} creates a set table containing all entities in the
#' reference table.
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
    .anvil_import_table(table, table_name, overwrite)
}


#' @rdname anvil_import
#' @export
anvil_import_set <- function(table, table_name, overwrite=FALSE) {
    # check that table is named correctly (ends in '_set')
    if (!(grepl("_set$", table_name))) {
        stop("Name of set table must end in '_set'")
    }
    
    # check that reference table exists in anvil
    anvil_tables <- avtables()
    ref_name <- sub("_set$", "", table_name)
    if (!(ref_name %in% anvil_tables$table)) {
        stop("Must import table ", ref_name, " before set table ", table_name)
    }
    
    # check that all entities in set exist in reference table
    ref_table <- avtable(ref_name)
    entity_id <- paste0(ref_name, "_id")
    anvil_entity <- ref_table[[entity_id]]
    this_entity <- table[[entity_id]]
    if (length(setdiff(this_entity, anvil_entity) > 0)) {
        stop("Some entities in set table not present in ", ref_name)
    }
    
    # check if set already exists
    set_id <- paste0(table_name, "_id")
    if (table_name %in% anvil_tables$table) {
        anvil_set <- avtable(table_name)
        anvil_set_ids <- anvil_set[[set_id]]
        overlaps <- intersect(anvil_set_ids, table[[set_id]])
        if (length(overlaps) > 0) {
            if (overwrite) {
                message("  Overwriting sets: ", paste(overlaps, collapse=", "))
                # must delete set before writing, otherwise entities will be duplicated
                avtable_delete_values(table_name, overlaps)
            } else {
                stop("Some sets in table '", table_name, "' already exist\n",
                     "  Set overwrite=TRUE to overwrite these rows")
            }
        }
    }
    
    # can't use avtable_import as it checks that entity_id is unique
    #.anvil_import_table(table, table_name, overwrite)
    avtable_import_set(table, origin=ref_name, set=set_id, member=entity_id)
}


#' @rdname anvil_import
#' @return \code{create_set_all} returns a tibble with one set, 'all',
#' containing all entities in \code{table}.
#' @importFrom dplyr tibble
#' @export
create_set_all <- function(table, table_name) {
    entity_id <- paste0(table_name, "_id")
    set_id <- paste0(table_name, "_set_id")
    df <- tibble(set_id="all", id=table[[entity_id]])
    names(df) <- c(set_id, entity_id)
    return(df)
}


.anvil_import_table <- function(table, table_name, overwrite=FALSE) {
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

