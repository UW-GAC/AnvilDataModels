context("import functions")

test_that("import tsv", {
    tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
    table_names <- c("subject", "phenotype", "sample", "sample_set", "file")
    x <- tsv_to_dm(tsv)
    expect_true(is_dm(x))
    expect_equal(names(dm_get_tables(x)), table_names)
    expect_equal(nrow(dm_get_all_pks(x)), 4)
    expect_equal(nrow(dm_get_all_fks(x)), 4)
})

test_that("tsv to dbml", {
    tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
    table_names <- c("subject", "phenotype", "sample", "file")
    tmp <- tempfile()
    tsv_to_dbml(tsv, tmp)
    dbml <- readLines(tmp)
    expect_true(all(paste("Table", table_names, "{") %in% dbml))
    expect_true("  indexes {" %in% dbml)
    expect_true("enum sex {" %in% dbml)
    unlink(tmp)
})
