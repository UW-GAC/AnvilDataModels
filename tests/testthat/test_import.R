context("import functions")

test_that("import tsv", {
    tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
    table_names <- c("subject", "phenotype", "sample", "sample_set", "file")
    x <- tsv_to_dm(tsv)
    expect_true(is_dm(x))
    expect_equal(names(dm_get_tables(x)), table_names)
    expect_equal(nrow(dm_get_all_pks(x)), 5)
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

test_that("multiple tsv files", {
    tsv1 <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
    dm2 <- tibble(entity="Table",
                  table="dataset",
                  column=c("dataset_id", "sample_set_id"),
                  type="varchar")
    tsv2 <- tempfile()
    readr::write_tsv(dm2, file=tsv2)
    dat <- .read_data_model(c(tsv1, tsv2))
    expect_true(all(c("subject", "dataset") %in% dat$table))
    unlink(tsv2)
})

test_that("field_value", {
    dat <- tibble(field=c("char", "int", "float", "bool", "enum", "date"),
                  value=c("a", "1", "1.1", "TRUE", "X", "2020-01-01"))
    model <- as_dm(list(a=tibble(char=character(), int=integer(), float=numeric(),
                             bool=logical(), enum=factor(levels=c("X","Y")),
                             date=ymd())))
    dat2 <- transpose_field_value(dat, table_name="a", model=model)
    expect_equal(names(dat2), dat$field)
    expect_equivalent(sapply(dat2, as.character), dat$value)
})

test_that("no tables", {
    tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
    tmp <- tempfile()
    mod <- readr::read_tsv(tsv, show_col_types=FALSE) %>%
        filter(entity != "Table")
    readr::write_tsv(mod, tmp)
    expect_error(tsv_to_dm(tmp), "Data model must contain Table entities")
    unlink(tmp)
})

test_that("no enums", {
    tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
    tmp <- tempfile()
    mod <- readr::read_tsv(tsv, show_col_types=FALSE) %>%
        filter(entity != "enum")
    readr::write_tsv(mod, tmp)
    expect_error(tsv_to_dm(tmp), "Undefined data type")
    
    # remove column that required the enum
    mod <- filter(mod, type != "sex")
    readr::write_tsv(mod, tmp)
    x <- tsv_to_dm(tmp)
    expect_true(is_dm(x))
    
    unlink(tmp)
})

test_that("no meta", {
    tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
    tmp <- tempfile()
    mod <- readr::read_tsv(tsv, show_col_types=FALSE) %>%
        filter(entity != "meta")
    readr::write_tsv(mod, tmp)
    x <- tsv_to_dm(tmp)
    expect_true(all(!attr(x, "required")))
    unlink(tmp)
})
