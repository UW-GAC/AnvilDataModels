context("import functions")

test_that("import tsv", {
    tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
    table_names <- c("subject", "sample", "sample_set", "dataset", "file", "array_dataset")
    x <- tsv_to_dm(tsv)
    expect_true(is_dm(x))
    expect_equal(names(dm_get_tables(x)), table_names)
    expect_equal(nrow(dm_get_all_pks(x)), 6)
    expect_equal(nrow(dm_get_all_fks(x)), 5)
})

test_that("tsv to dbml", {
    tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
    table_names <- c("subject", "sample", "sample_set", "dataset", "file", "array_dataset")
    tmp <- tempfile()
    tsv_to_dbml(tsv, tmp)
    dbml <- readLines(tmp)
    expect_true(all(paste("Table", table_names, "{") %in% dbml))
    unlink(tmp)
})
