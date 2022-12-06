context("import functions")

test_that("import json", {
    json <- system.file("extdata", "data_model.json", package="AnvilDataModels")
    table_names <- c("subject", "phenotype", "sample", "sample_set", "file")
    x <- json_to_dm(json)
    expect_true(is_dm(x))
    expect_equal(names(dm_get_tables(x)), table_names)
    expect_equal(nrow(dm_get_all_pks(x)), 5)
    expect_equal(nrow(dm_get_all_fks(x)), 4)
    expect_equal(attr(x, "data_model_version"), "0")
})

test_that("json to dbml", {
    json <- system.file("extdata", "data_model.json", package="AnvilDataModels")
    table_names <- c("subject", "phenotype", "sample", "file")
    tmp <- tempfile()
    json_to_dbml(json, tmp)
    dbml <- readLines(tmp)
    expect_true(all(paste("Table", table_names, "{") %in% dbml))
    expect_true("  indexes {" %in% dbml)
    expect_true("// version 0" %in% dbml)
    unlink(tmp)
})

test_that("multiple json files", {
    json1 <- system.file("extdata", "data_model.json", package="AnvilDataModels")
    json2 <- system.file("extdata", "data_model_auto_id.json", package="AnvilDataModels")
    dat <- .read_data_model(c(json1, json2))
    expect_equal(length(dat$tables), 7)
    expect_true(all(c("subject", "analysis") %in% .named_elements(dat$tables, "table", "table")))
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

test_that("field_value_extra_cols", {
    dat <- tibble(field=c("char", "int", "float", "bool", "other"),
                  value=c("a", "1", "1.1", "TRUE", "hi"))
    model <- as_dm(list(a=tibble(char=character(), int=integer(), float=numeric(),
                                 bool=logical())))
    dat2 <- transpose_field_value(dat, table_name="a", model=model)
    expect_equal(names(dat2), dat$field)
    expect_equal(dat2$other, "hi")
})

test_that("no tables", {
    tmp <- tempfile()
    writeLines('{"a": "b"}', tmp)
    expect_error(json_to_dm(tmp), "Data model must contain Table entities")
    unlink(tmp)
})

test_that("auto ids", {
    json <- system.file("extdata", "data_model_auto_id.json", package="AnvilDataModels")
    table_names <- c("analysis", "file")
    x <- json_to_dm(json)
    expect_true(is_dm(x))
    expect_equal(names(dm_get_tables(x)), table_names)
    expect_equal(nrow(dm_get_all_fks(x)), 1)
    
    tmp <- tempfile()
    json_to_dbml(json, tmp)
    dbml <- readLines(tmp)
    expect_false(any(grepl("ref: from: ", dbml, fixed=TRUE)))
    unlink(tmp)
})


test_that("conditional columns", {
    json <- system.file("extdata", "data_model_conditional.json", package="AnvilDataModels")
    x <- json_to_dm(json)
    expect_setequal(attr(x$t1, "required"), c("t1_id", "condition", "variable"))
    expect_setequal(attr(x$t1, "conditions"), c("condition = TRUE", "variable = yes"))
    expect_setequal(attr(x$t2, "required"), "t2_id")
    expect_setequal(attr(x$t2, "conditions"), character())
})


test_that("conditional tables", {
    json <- system.file("extdata", "data_model_conditional.json", package="AnvilDataModels")
    x <- json_to_dm(json)
    expect_equal(attr(x, "required"), "t1")
    expect_equal(attr(x, "conditions"), c(t3="t2"))
})
