context("anvil functions")

test_that("add entity id", {
    tables <- .tables()
    model <- .model()
    table_names <- c("subject", "phenotype", "sample", "file")
    for (t in table_names) {
        ent <- add_entity_id(tables[[t]], t, model)
        expect_true(paste0(t, "_id") %in% names(ent))
    }
})
