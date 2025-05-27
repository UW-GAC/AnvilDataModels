#' Edit data tables
#' 
#' Edit a set of data tables while maintaining consistency among foreign keys
#' 
#' When calling \code{delete_rows} and \code{keep_rows}, a side effect is that only rows that are
#' related via foreign keys to rows in \code{table_name} are kept. Therefore,
#' \code{table_name} should always be the primary table.
#' 
#' @name edit_data_tables
#' @param pk_value Value of the primary key to filter on (either to keep or delete)
#' @param table_name Name of the table to filter 
#' @param pk_name Name of the primary key column. Defaults to the table name + "_id".
#' @param tables Named list of data tables
#' @param model \code{\link{dm}} object describing data model
#' @return \code{\link{dm}} object with filtered tables
#' @importFrom dplyr filter sym 
#' @importFrom rlang :=
#' @export
delete_rows <- function(pk_value, table_name, pk_name=paste0(table_name, "_id"), tables, model) {
    tables_dm <- .create_dm_object(tables, model)
    filtered_tables <- dm_filter(tables_dm, {{table_name}} := (!(!!sym(pk_name) %in% pk_value)))
    .filter_set_tables(filtered_tables, tables_dm)
}


#' @name edit_data_tables
#' @export
keep_rows <- function(pk_value, table_name, pk_name=paste0(table_name, "_id"), tables, model) {
    tables_dm <- .create_dm_object(tables, model)
    filtered_tables <- dm_filter(tables_dm, {{table_name}} := (!!sym(pk_name) %in% pk_value))
    filtered_tables
}


.create_dm_object <- function(tables, model) {
    tables_dm <- as_dm(tables)
    pks <- dm_get_all_pks(model)
    fks <- dm_get_all_fks(model)
    
    for (k in 1:nrow(pks)) {
        if (pks$table[k] %in% names(tables_dm)) {
            tables_dm <- dm_add_pk(tables_dm, !!pks$table[k], !!pks$pk_col[[k]])
        }
    }
    for (k in 1:nrow(fks)) {
        if (fks$child_table[k] %in% names(tables_dm) & fks$parent_table[k] %in% names(tables_dm)) {
            tables_dm <- dm_add_fk(tables_dm, 
                                   !!fks$child_table[k], !!fks$child_fk_cols[[k]],
                                   !!fks$parent_table[k], !!fks$parent_key_cols[[k]])
        }
    }
    tables_dm
}


.filter_set_tables <- function(filtered_tables_dm, original_tables_dm) {
    # if we use dm_filter on the set table, only rows that are present 
    # in the set table will be kept
    tables <- lapply(filtered_tables_dm, function(x) x)
    set_tables <- names(tables)[grepl("_set$", names(tables))]
    for (t in set_tables) {
        origin_id <- sub("_set$", "_id", t)
        incomplete_sets <- original_tables_dm[[t]] %>%
            filter(!(!!sym(origin_id) %in% filtered_tables_dm[[t]][[origin_id]]))
        set_id_col <- paste0(t, "_id")
        incomplete_set_ids <- incomplete_sets[[set_id_col]]
        #tables <- dm_filter(tables, {{t}} := (!(!!sym(set_id_col) %in% incomplete_set_ids)))
        for (t2 in names(tables)) {
            if (set_id_col %in% names(tables[[t2]])) {
                tables[[t2]] <- filter(tables[[t2]], (!(!!sym(set_id_col) %in% incomplete_set_ids)))
            }
        }
    }
    tables
}

#' @importFrom tidyr nest
.collapse_set_table <- function(x, set_id) {
    x2 <- nest(x, .by=!!set_id)
    for (i in 1:nrow(x2)) {
        x2$data[[i]] <- unlist(x2$data[[i]], use.names=FALSE)
    }
    names(x2)[2] <- setdiff(names(x), set_id)
    x2
}


