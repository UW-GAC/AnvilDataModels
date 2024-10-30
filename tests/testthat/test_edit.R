context("edit functions")

test_that("collapse set table", {
    x <- .tables()[["sample_set"]]
    xt <- tibble(sample_set_id = c("set1", "set2"),
                 sample_id = list(paste0("sample", 1:3),
                                  paste0("sample", 2:6)))
    expect_equal(.collapse_set_table(x, "sample_set_id"), xt)
})


test_that("create dm", {
    tables <- .tables()
    model <- .model()
    tables_dm <- .create_dm_object(tables, model)
    expect_equal(dm_get_all_pks(model), dm_get_all_pks(tables_dm))
    expect_equal(dm_get_all_fks(model), dm_get_all_fks(tables_dm))
})


test_that("remove rows - no sets", {
    table_names <- c("subject", "phenotype", "sample", "file")
    files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
    tables <- read_data_tables(files, table_names=table_names, quiet=TRUE)
    json <- system.file("extdata", "data_model_no_sets.json", package="AnvilDataModels")
    model <- json_to_dm(json)
    
    xt <- list(
        subject = filter(tables$subject, subject_id != "subject1"),
        phenotype = filter(tables$phenotype, subject_id != "subject1"),
        sample = filter(tables$sample, subject_id != "subject1"),
        file = filter(tables$file, !(sample_id %in% c("sample1", "sample1a")))
    )
    
    filtered_tables <- delete_rows(pk_value="subject1", table_name="subject", tables=tables, model=model)
    for (t in names(xt)) expect_equal(filtered_tables[[t]], xt[[t]])
})


test_that("remove rows - sets", {
    tables <- .tables()
    model <- .model()
    
    xt <- list(
        subject = filter(tables$subject, subject_id != "subject1"),
        phenotype = filter(tables$phenotype, subject_id != "subject1"),
        sample = filter(tables$sample, subject_id != "subject1"),
        #sample_set = filter(tables$sample_set, sample_id != "sample1"),
        sample_set = filter(tables$sample_set, sample_set_id != "set1"),
        file = filter(tables$file, !(sample_id %in% c("sample1", "sample1a")))
    )
    
    filtered_tables <- delete_rows(pk_value="subject1", table_name="subject", tables=tables, model=model)
    for (t in names(xt)) expect_equal(filtered_tables[[t]], xt[[t]])
})


test_that("remove rows - sets with reference", {
    json <- system.file("extdata", "data_model_set_fk.json", package="AnvilDataModels")
    model <- json_to_dm(json)
    
    table_names <- c("sample", "sample_set", "file_multi")
    files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
    tables <- read_data_tables(files, table_names=table_names, quiet=TRUE)
    tables$file_multi <- tables$file_multi[1:2,]
    
    xt <- list(
        sample = filter(tables$sample, sample_id != "sample1"),
        sample_set = filter(tables$sample_set, sample_set_id != "set1"),
        file_multi = filter(tables$file_multi, sample_set_id != "set1")
    )
    
    filtered_tables <- delete_rows(pk_value="sample1", table_name="sample", tables=tables, model=model)
    for (t in names(xt)) expect_equal(filtered_tables[[t]], xt[[t]])
})
