context("check functions")

test_that("read files", {
    table_names <- c("subject", "phenotype", "sample", "sample_set", "file")
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
    tables$phenotype <- NULL
    tables$foo <- dplyr::tibble()
    expect_equal(check_table_names(tables, model),
                 list(missing_required_tables="sample",
                      missing_optional_tables="phenotype",
                      extra_tables="foo"))
})


test_that("check column names", {
    tables <- .tables()
    model <- .model()
    expect_equal(check_column_names(tables, model), lapply(tables, function(x) NULL))
    
    tables$sample$sample_id <- NULL
    tables$sample$age_at_sample_collection <- NULL
    tables$sample$foo <- "a"
    expect_equal(check_column_names(tables, model)$sample,
                 list(missing_required_columns="sample_id",
                      missing_optional_columns="age_at_sample_collection",
                      extra_columns="foo"))
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
    set.seed(10)
    x$subject$dbgap_submission <- sample(1:2, nrow(x$subject), replace=TRUE)
    expect_equal(check_column_types(x, model)$subject$dbgap_submission,
                 "Some values of subject.dbgap_submission not compatible with boolean type: 2")
    
    # char instead of int
    x <- tables
    set.seed(10)
    x$sample$age_at_sample_collection <- sample(c("a", "b"), nrow(x$sample), replace=TRUE)
    expect_equal(check_column_types(x, model)$sample$age_at_sample_collection,
                 "Some values of sample.age_at_sample_collection not compatible with integer type: a, b")
    
    # float instead of int
    x <- tables
    x$sample$age_at_sample_collection <- 10.5
    expect_equal(check_column_types(x, model)$sample$age_at_sample_collection,
                 "Some values of sample.age_at_sample_collection not compatible with integer type: 10.5")
    
    # int instead of float - should be ok
    x <- tables
    x$phenotype$height <- 10
    expect_null(check_column_types(x, model)$phenotype$height)
    
    # not a date
    x <- tables
    x$sample$date_of_sample_processing <- "a"
    expect_equal(check_column_types(x, model)$sample$date_of_sample_processing,
                 "Some values of sample.date_of_sample_processing not compatible with date type: a")
    
    # not a datetime
    x <- tables
    x$file$file_timestamp <- "2000-01-01"
    expect_equal(check_column_types(x, model)$file$file_timestamp,
                 "Some values of file.file_timestamp not compatible with datetime type: 2000-01-01")
    
    # wrong levels for enum
    x <- tables
    x$subject$reported_sex <- "A"
    expect_equal(check_column_types(x, model)$subject$reported_sex,
                 "Some values of subject.reported_sex not compatible with enum type: A. Allowed values: F, M, X")
    
    # integer instead of factor
    x <- tables
    x$subject$reported_sex <- 1L
    expect_equal(check_column_types(x, model)$subject$reported_sex,
                 "Some values of subject.reported_sex not compatible with enum type: 1. Allowed values: F, M, X")
})


test_that("check primary keys", {
    tables <- .tables()
    model <- .model()
    expect_equal(check_primary_keys(tables, model)$found_keys$problem, rep("", 4))
    
    # non-unique key
    x <- tables
    x$sample$sample_id[1] <- x$sample$sample_id[2]
    expect_true("has duplicate values: sample1 (2)" %in%
                    check_primary_keys(x, model)$found_keys$problem)
    
    # subset of model in tables
    tables$file <- NULL
    expect_equal(check_primary_keys(tables, model)$found_keys$problem, rep("", 3))
})


test_that("missing primary key", {
    tables <- .tables()
    model <- .model()
    
    x <- tables
    x$sample$sample_id <- NULL
    chk <- check_primary_keys(x, model)
    expect_equal(chk$found_keys$problem, rep("", 3))
    expect_equal(chk$missing_keys, list(sample="sample_id"))
})


test_that("check foreign keys", {
    tables <- .tables()
    model <- .model()
    chk <- check_foreign_keys(tables, model)$found_keys
    fk_ind <- which(chk$kind == "FK")
    expect_equal(length(fk_ind), 4)
    expect_setequal(chk$problem, "")
    
    # missing value of foreign key in reference table
    x <- tables
    x$subject <- filter(x$subject, subject_id != "subject1")
    chk <- as_tibble(check_foreign_keys(x, model)$found_keys) %>%
        filter(kind == "FK")
    expect_equal(unlist(chk$columns) == "subject_id", grepl("subject1", chk$problem))
    
    # subset of model in tables
    tables$file <- NULL
    chk <- check_foreign_keys(tables, model)$found_keys
    fk_ind <- which(chk$kind == "FK")
    expect_equal(length(fk_ind), 3)
})


test_that("missing foreign key", {
    tables <- .tables()
    model <- .model()
    
    x <- tables
    x$sample$sample_id <- NULL
    x$subject$subject_id <- NULL
    x$phenotype$subject_id <- NULL
    chk <- check_foreign_keys(x, model)
    expect_equal(length(chk$found_keys$problem), 0)
    expect_equal(chk$missing_keys, 
                 list(phenotype="subject_id", subject="subject_id", sample="sample_id"))
})


test_that("missing data", {
    model <- list(tables=list(
        list(table="tbl",
             columns=list(
                 list(column="s",
                      data_type="string"),
                 list(column="i",
                      data_type="integer"),
                 list(column="f",
                      data_type="float"),
                 list(column="b",
                      data_type="boolean"),
                 list(column="d",
                      data_type="date"),
                 list(column="e",
                      data_type="enumeration",
                      enumerations=letters[1:2])
             ))
    )) %>%
        jsonlite::toJSON(auto_unbox=TRUE, unbox=TRUE)

    modfile <- tempfile()
    write(model, modfile)
    model <- json_to_dm(modfile)
    
    dat <- tibble(
        s=c("a", NA),
        i=c(NA, 2),
        f=c(1.5, NA),
        b=c(NA, TRUE),
        d=c("2000-01-01", NA),
        e=c(NA, "a")
    )
    datfile <- tempfile()
    readr::write_tsv(dat, datfile, na="")
    tables <- read_data_tables(datfile, table_names="tbl")
    chk <- check_column_types(tables, model)
    expect_true(all(sapply(chk$tbl, is.null)))
    
    unlink(c(modfile, datfile))
})


test_that("conditional columns - parsing", {
    json <- system.file("extdata", "data_model_conditional.json", package="AnvilDataModels")
    x <- json_to_dm(json)
    dat <- tibble(t1_id=1:2,
                  condition=c(TRUE, FALSE),
                  if_condition=c("a", "b"),
                  variable=c("yes", "no"),
                  if_variable=c("a", "b"))
    chk <- .parse_required_columns(dat, x$t1)
    expect_setequal(chk$required, names(dat))
    expect_equal(chk$optional, "something")
    
    dat$condition[1] <- FALSE
    dat$variable[1] <- "no"
    dat$something <- "a"
    chk <- .parse_required_columns(dat, x$t1)
    expect_setequal(chk$required, c("t1_id", "condition", "variable"))
    expect_setequal(chk$optional, c("if_condition", "if_variable", "something"))
})

test_that("conditional columns - check", {
    json <- system.file("extdata", "data_model_conditional.json", package="AnvilDataModels")
    x <- json_to_dm(json)
    dat <- tibble(t1_id=1:2,
                  condition=c(TRUE, FALSE),
                  if_condition=c("a", "b"),
                  variable=c("yes", "no"),
                  if_variable=c("a", "b"))
    chk <- check_column_names(tables=list(t1=dat), model=x)
    expect_setequal(chk$t1$missing_required_columns, character())
    expect_setequal(chk$t1$missing_optional_columns, "something")
    
    dat2 <- tibble(t1_id=1:2,
                  condition=c(FALSE, FALSE),
                  variable=c("no", "no"))
    chk <- check_column_names(tables=list(t1=dat2), model=x)
    expect_setequal(chk$t1$missing_required_columns, character())
    expect_setequal(chk$t1$missing_optional_columns, c("if_condition", "if_variable", "something"))
    
    dat$something <- "a"
    chk <- check_column_names(tables=list(t1=dat), model=x)
    expect_null(chk$t1)
})

test_that("conditional tables - parsing", {
    json <- system.file("extdata", "data_model_conditional.json", package="AnvilDataModels")
    x <- json_to_dm(json)
    chk <- .parse_required_tables(c("t1", "t2", "t3"), x)
    expect_setequal(chk$required, c("t1", "t3"))
    expect_setequal(chk$optional, c("t2"))
    
    chk <- .parse_required_tables(c("t1"), x)
    expect_setequal(chk$required, c("t1"))
    expect_setequal(chk$optional, c("t2", "t3"))
})

test_that("conditional tables - check", {
    json <- system.file("extdata", "data_model_conditional.json", package="AnvilDataModels")
    x <- json_to_dm(json)
    chk <- check_table_names(tables=list(t1=tibble()), model=x)
    expect_setequal(chk$missing_required_tables, character())
    expect_setequal(chk$missing_optional_tables, c("t2", "t3"))
    
    chk <- check_table_names(tables=list(t1=tibble(), t2=tibble()), model=x)
    expect_setequal(chk$missing_required_tables, c("t3"))
    expect_setequal(chk$missing_optional_tables, character())
    
    chk <- check_table_names(tables=list(t1=tibble(), t2=tibble(), t3=tibble()), model=x)
    expect_null(chk)
})

test_that("foreign keys with sets", {
    json <- system.file("extdata", "data_model_set_fk.json", package="AnvilDataModels")
    model <- json_to_dm(json)
    
    table_names <- c("sample", "sample_set", "file_multi")
    files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
    tables <- read_data_tables(files, table_names=table_names, quiet=TRUE)
    
    chk <- check_foreign_keys(tables, model)
    expect_setequal(chk$found_keys$problem, "")
    expect_equal(chk$set_key_problems, 
                 list("file_multi.sample_set_id"="Not all values present in sample_set.sample_set_id"))
})

test_that("enumeration with delimiter", {
    json <- system.file("extdata", "data_model_delim.json", package="AnvilDataModels")
    x <- json_to_dm(json)
    dat <- tibble(t1_id=1:3,
                  value=c("A", "A | B", "A|B|C"))
    chk <- check_column_types(tables=list(t1=dat), model=x)
    expect_null(chk$t1$value)
    
    dat <- tibble(t1_id=1:3,
                  value=c("A", "A|D", "A|B|C"))
    chk <- check_column_types(tables=list(t1=dat), model=x)
    expect_equal("Some values of t1.value not compatible with enum type: D. Allowed values: A, B, C", chk$t1$value)
    
    dat <- tibble(t1_id=1:3,
                  value=NA)
    chk <- check_column_types(tables=list(t1=dat), model=x)
    expect_null(chk$t1$value)
})

test_that("no keys", {
    json <- system.file("extdata", "data_model_no_keys.json", package="AnvilDataModels")
    x <- json_to_dm(json)
    dat <- tibble(a=letters, b=LETTERS)
    chk <- check_primary_keys(tables=list(t1=dat), model=x)
    expect_equal(length(chk$found_keys$problem), 0)
    expect_equal(length(chk$missing_keys), 0)
    chk <- check_foreign_keys(tables=list(t1=dat), model=x)
    expect_equal(length(chk$found_keys$problem), 0)
    expect_equal(length(chk$missing_keys), 0)
})
