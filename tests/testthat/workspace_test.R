context("tests that can only be run on AnVIL")

test_that("upload example files", {
  table_names <- c("subject", "phenotype", "sample", "sample_set", "file")
  files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
  bucket <- AnVIL::avbucket()
  lapply(files, AnVIL::gsutil_cp, bucket)
})

test_that("table with primary key matching table name", {
  tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
  model <- tsv_to_dm(tsv)
  table_name <- "subject"
  file <- system.file("extdata", paste0(table_name, ".tsv"), package="AnvilDataModels")
  tables <- read_data_tables(file, table_name, quiet=TRUE)
  x <- tables[[table_name]]
  anvil_import_table(x, table_name, model, overwrite=TRUE)
  
  chk <- avtable(table_name)
  pk <- paste0(table_name, "_id")
  expect_setequal(chk[[pk]], x[[pk]])
  # avtable reads logical as character
  for (i in names(x)) if (is.logical(x[[i]])) x[[i]] <- as.character(x[[i]])
  expect_equivalent(chk[match(x[[pk]], chk[[pk]]), names(x)], x)
  
  expect_error(anvil_import_table(x, table_name, model, overwrite=FALSE),
               "Some entities in table 'subject' already exist")
})

test_that("table with primary key not matching table name", {
  tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
  model <- tsv_to_dm(tsv)
  table_name <- "file"
  file <- system.file("extdata", paste0(table_name, ".tsv"), package="AnvilDataModels")
  tables <- read_data_tables(file, table_name, quiet=TRUE)
  x <- tables[[table_name]]
  anvil_import_table(x, table_name, model, overwrite=TRUE)
  
  chk <- avtable(table_name)
  pk <- paste0(table_name, "_id")
  expect_setequal(names(chk), c(pk, names(x)))
  expect_equal(chk[[pk]], chk$md5)
})

test_that("table with >1 primary key", {
  tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
  model <- tsv_to_dm(tsv)
  table_name <- "phenotype"
  file <- system.file("extdata", paste0(table_name, ".tsv"), package="AnvilDataModels")
  tables <- read_data_tables(file, table_name, quiet=TRUE)
  x <- tables[[table_name]]
  anvil_import_table(x, table_name, model, overwrite=TRUE)
  
  chk <- avtable(table_name)
  pk <- paste0(table_name, "_id")
  expect_setequal(names(chk), c(pk, names(x)))
  expect_equal(chk[[pk]], paste(chk$subject_id, chk$visit_id, sep="_"))
})

test_that("set", {
  tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
  model <- tsv_to_dm(tsv)
  table_name <- "sample"
  set_name <- paste0(table_name, "_set")
  files <- system.file("extdata", paste0(c(table_name, set_name), ".tsv"), package="AnvilDataModels")
  names(files) <- c(table_name, set_name)
  tables <- read_data_tables(files, quiet=TRUE)
  x <- tables[[table_name]]
  set <- tables[[set_name]]
  
  # can't import non-set table as set
  expect_error(anvil_import_set(x, table_name, overwrite=TRUE), 
               "Name of set table must end in '_set'")
  
  # can't import set without table
  chk <- avtable(table_name)
  avtable_delete_values("sample", chk$sample_id)
  expect_error(anvil_import_set(set, set_name, overwrite=TRUE), 
               "Must import table sample before set table sample_set")
  
  anvil_import_table(x, table_name, model, overwrite=TRUE)
  
  # wrong set values
  set2 <- tibble(sample_set_id="a", sample_id="b")
  expect_error(anvil_import_set(set2, set_name, overwrite=TRUE),
               "Some entities in set table not present in sample")
  
  anvil_import_set(set, set_name, overwrite=TRUE)
  chk <- avtable(table_name)
  chk_set <- avtable(set_name)
  expect_true(all(chk$set$sample_id %in% chk$sample_id))
  
  set_all <- create_set_all(x, table_name)
  anvil_import_set(set_all, set_name, overwrite=TRUE)
  chk_set <- avtable(set_name)
  expect_setequal(chk_set$sample_set_id, c("set1", "set2", "all"))
})
