
test_that("auto ids", {
    tsv <- system.file("extdata", "data_model_auto_id.tsv", package="AnvilDataModels")
    model <- tsv_to_dm(tsv)
    table_name <- "analysis"
    file <- system.file("extdata", "analysis1.tsv", package="AnvilDataModels")
    tables <- read_data_tables(file, table_names=table_name, quiet=TRUE)
    tables2 <- add_auto_columns(tables[[table_name]], table_name=table_name, model)
    expect_true("analysis_id" %in% names(tables2))
})

test_that("table with no auto ids", {
    tsv <- system.file("extdata", "data_model_auto_id.tsv", package="AnvilDataModels")
    model <- tsv_to_dm(tsv)
    table_name <- "file"
    file <- system.file("extdata", "analysis_file1.tsv", package="AnvilDataModels")
    tables <- read_data_tables(file, table_names=table_name, quiet=TRUE)
    tables2 <- add_auto_columns(tables[[table_name]], table_name=table_name, model)
    expect_equal(tables$file, tables2)
})

test_that("missing columns for auto ids", {
    tsv <- system.file("extdata", "data_model_auto_id.tsv", package="AnvilDataModels")
    model <- tsv_to_dm(tsv)
    table_name <- "analysis"
    file <- system.file("extdata", "analysis1.tsv", package="AnvilDataModels")
    tables <- read_data_tables(file, table_names=table_name, quiet=TRUE)
    
    tables$analysis$outcome <- NULL
    expect_error(add_auto_columns(tables[[table_name]], table_name=table_name, model), 
                 "Cannot create id")
    
    expect_warning(tables2 <- add_auto_columns(tables[[table_name]], table_name=table_name, model,
                                error_on_missing=FALSE),
                   "Cannot create id")
    expect_false("analysis_id" %in% names(tables2))
})
