anvil_import <- function(tables, model) {
    for (t in names(tables)) {
        pk <- dm_get_all_pks(model, table=t)$pk_col
        if (length(pk) == 0) stop("cannot add table no primary key defined")
        if (length(pk) > 1) stop("cannot add table; > 1 primary key")
        pk <- pk[[1]]
        if (length(pk) > 1) stop("compound keys not supported yet")
        avtable_import(tables[[t]], entity=pk)
    }
}
