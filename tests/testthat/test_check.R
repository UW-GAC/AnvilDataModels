context("check functions")

.tables <- function() {
    table_names <- c("subject", "phenotype", "sample", "file")
    files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
    read_data_tables(files, table_names=table_names, quiet=TRUE)
}

.model <- function() {
    tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
    tsv_to_dm(tsv)
}

test_that("read files", {
    table_names <- c("subject", "phenotype", "sample", "file")
    files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
    names(files) <- table_names
    x <- read_data_tables(files, quiet=TRUE)
    expect_equal(names(x), table_names)
})


test_that("check table names", {
    tables <- .tables()
    model <- .model()
    expect_null(check_table_names(tables, model))
    
    tables$sample <- NULL
    tables$foo <- dplyr::tibble()
    expect_equal(check_table_names(tables, model),
                 list(missing_tables="sample", extra_tables="foo"))
})


test_that("check column names", {
    tables <- .tables()
    model <- .model()
    expect_equal(check_column_names(tables, model), lapply(tables, function(x) NULL))
    
    tables$sample$sample_id <- NULL
    tables$sample$foo <- "a"
    expect_equal(check_column_names(tables, model)$sample,
                 list(missing_columns="sample_id", extra_columns="foo"))
})


test_that("check column types", {
    tables <- .tables()
    model <- .model()
    expect_equal(check_column_types(tables, model), 
                 lapply(tables, function(x) lapply(x, function(y) NULL)))
    
    # integer instead of character - should be ok
    x <- tables
    x$subject$subject_id <- 1:10
    expect_null(check_column_types(x, model)$subject$subject_id)
    
    # incompatible boolean values
    x <- tables
    x$subject$dbgap_submission <- sample(1:2, nrow(x$subject), replace=TRUE)
    expect_equal(check_column_types(x, model)$subject$dbgap_submission,
                 "Some values of subject.dbgap_submission not compatible with boolean type")
    
    # char instead of int
    x <- tables
    x$sample$age_at_sample_collection <- "a"
    expect_equal(check_column_types(x, model)$sample$age_at_sample_collection,
                 "Some values of sample.age_at_sample_collection not compatible with integer type")
    
    # float instead of int
    x <- tables
    x$sample$age_at_sample_collection <- 10.5
    expect_equal(check_column_types(x, model)$sample$age_at_sample_collection,
                 "Some values of sample.age_at_sample_collection not compatible with integer type")
    
    # int instead of float - should be ok
    x <- tables
    x$phenotype$height <- 10
    expect_null(check_column_types(x, model)$phenotype$height)
    
    # not a date
    x <- tables
    x$sample$date_of_sample_processing <- "a"
    expect_equal(check_column_types(x, model)$sample$date_of_sample_processing,
                 "Some values of sample.date_of_sample_processing not compatible with date type")
    
    # not a datetime
    x <- tables
    x$file$file_timestamp <- "2000-01-01"
    expect_equal(check_column_types(x, model)$file$file_timestamp,
                 "Some values of file.file_timestamp not compatible with datetime type")
    
    # wrong levels for enum
    x <- tables
    x$subject$reported_sex <- "A"
    expect_equal(check_column_types(x, model)$subject$reported_sex,
                 "Some values of subject.reported_sex not compatible with enum type")
    
    # integer instead of factor
    x <- tables
    x$subject$reported_sex <- 1L
    expect_equal(check_column_types(x, model)$subject$reported_sex,
                 "Some values of subject.reported_sex not compatible with enum type")
})
