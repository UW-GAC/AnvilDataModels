context("tests that can only be run on AnVIL")

test_that("upload example files", {
  table_names <- c("subject", "phenotype", "sample", "sample_set", "file")
  files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
  bucket <- AnVIL::avbucket()
  lapply(files, AnVIL::gsutil_cp, bucket)
})

test_that("table with primary key matching table name", {
  json <- system.file("extdata", "data_model.json", package="AnvilDataModels")
  model <- json_to_dm(json)
  table_name <- "subject"
  file <- system.file("extdata", paste0(table_name, ".tsv"), package="AnvilDataModels")
  tables <- read_data_tables(file, table_name, quiet=TRUE)
  x <- tables[[table_name]]
  anvil_import_table(x, table_name, model, overwrite=TRUE)
  
  chk <- AnVIL::avtable(table_name)
  pk <- paste0(table_name, "_id")
  expect_setequal(chk[[pk]], x[[pk]])
  # avtable reads logical as character
  for (i in names(x)) if (is.logical(x[[i]])) x[[i]] <- as.character(x[[i]])
  expect_equivalent(chk[match(x[[pk]], chk[[pk]]), names(x)], x)
  
  expect_error(anvil_import_table(x, table_name, model, overwrite=FALSE),
               "Some entities in table 'subject' already exist")
})

test_that("table with primary key not matching table name", {
  json <- system.file("extdata", "data_model.json", package="AnvilDataModels")
  model <- json_to_dm(json)
  table_name <- "file"
  file <- system.file("extdata", paste0(table_name, ".tsv"), package="AnvilDataModels")
  tables <- read_data_tables(file, table_name, quiet=TRUE)
  x <- tables[[table_name]]
  anvil_import_table(x, table_name, model, overwrite=TRUE)
  
  chk <- AnVIL::avtable(table_name)
  pk <- paste0(table_name, "_id")
  expect_setequal(names(chk), c(pk, names(x)))
  expect_equal(chk[[pk]], chk$md5)
})

test_that("table with >1 primary key", {
  json <- system.file("extdata", "data_model.json", package="AnvilDataModels")
  model <- json_to_dm(json)
  table_name <- "phenotype"
  file <- system.file("extdata", paste0(table_name, ".tsv"), package="AnvilDataModels")
  tables <- read_data_tables(file, table_name, quiet=TRUE)
  x <- tables[[table_name]]
  anvil_import_table(x, table_name, model, overwrite=TRUE)
  
  chk <- AnVIL::avtable(table_name)
  pk <- paste0(table_name, "_id")
  expect_setequal(names(chk), c(pk, names(x)))
  expect_equal(chk[[pk]], paste(chk$subject_id, chk$visit_id, sep="_"))
})

test_that("set", {
  json <- system.file("extdata", "data_model.json", package="AnvilDataModels")
  model <- json_to_dm(json)
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
  chk <- AnVIL::avtable(table_name)
  AnVIL::avtable_delete_values("sample", chk$sample_id)
  expect_error(anvil_import_set(set, set_name, overwrite=TRUE), 
               "Must import table sample before set table sample_set")
  
  anvil_import_table(x, table_name, model, overwrite=TRUE)
  
  # wrong set values
  set2 <- tibble::tibble(sample_set_id="a", sample_id="b")
  expect_error(anvil_import_set(set2, set_name, overwrite=TRUE),
               "Some entities in set table not present in sample")
  
  anvil_import_set(set, set_name, overwrite=TRUE)
  chk <- AnVIL::avtable(table_name)
  chk_set <- AnVIL::avtable(set_name)
  samples <- dplyr::bind_rows(chk_set$samples.items)$entityName
  expect_true(all(samples %in% chk$sample_id))
  
  set_all <- create_set_all(x, table_name)
  anvil_import_set(set_all, set_name, overwrite=TRUE)
  chk_set <- AnVIL::avtable(set_name)
  expect_setequal(chk_set$sample_set_id, c("set1", "set2", "all"))
  
  # can't overwrite existing set
  expect_error(anvil_import_set(set_all, set_name, overwrite=FALSE),
               "Some sets in table 'sample_set' already exist")
  
  # overwriting set doesn't duplicate values
  anvil_import_set(set_all, set_name, overwrite=TRUE)
  chk_set <- AnVIL::avtable(set_name)
  samples <- chk_set$samples.items[chk_set$sample_set_id == "all"][[1]]$entityName
  expect_true(sum(duplicated(samples)) == 0)
  
  # import all tables
  anvil_import_tables(tables, model, overwrite=TRUE)
})

test_that("upload error", {
    json <- system.file("extdata", "data_model.json", package="AnvilDataModels")
    model <- json_to_dm(json)
    table_names <- c("subject", "sample")
    files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
    tables <- read_data_tables(files, table_names, quiet=TRUE)
    tables$subject$subject_id[1] <- "a+b" # illegal character for primary key
    expect_error(anvil_import_tables(tables, model, overwrite=TRUE), "Import failed")
})


test_that("bucket paths exist", {
    json <- system.file("extdata", "data_model_files.json", package="AnvilDataModels")
    x <- json_to_dm(json)
    file1 <- system.file("extdata", "file.tsv", package="AnvilDataModels")
    bucket <- "gs://fc-efda2373-416d-45db-bde6-b3ad08bf9d79"
    bucket_path <- file.path(bucket, basename(file1))
    #AnVIL::gsutil_cp(file1, bucket_path)
    
    # valid uri but file does not exist
    dat <- tibble(t1_id=1:2,
                  file1=c(bucket_path, file.path(bucket, "foo")))
    chk <- check_bucket_paths(tables=list(t1=dat), model=x)
    expect_equal(chk$t1$file1, paste("Bucket paths in t1.file1 do not exist:", file.path(bucket, "foo")))
    
    # string that is not valid uri
    dat[2,"file1"] <- "foo"
    chk <- check_bucket_paths(tables=list(t1=dat), model=x)
    expect_equal(chk$t1$file1, "Bucket paths in t1.file1 do not exist: foo")
    
    # everything ok
    dat <- dat[1,]
    chk <- check_bucket_paths(tables=list(t1=dat), model=x)
    expect_equal(chk, list(t1=list(file1=NULL)))
    
    # missing values
    dat <- tibble(t1_id=1:2,
                  file1=NA)
    chk <- check_bucket_paths(tables=list(t1=dat), model=x)
    expect_equal(chk, list(t1=list(file1=NULL)))
    
    # multiple buckets and multiple tables
    dat <- tibble(t1_id=1:2,
                  file1=c(bucket_path, file.path(bucket, "foo")),
                  file2=file.path("gs://fc-995e5705-8dcb-410b-987e-c05b47d0c580",
                                  c("TEST_populations.tsv", "TEST_populations.txt")))
    tables <- list(t1=dat, t2=tibble(t1_id=1:2))
    chk <- check_bucket_paths(tables=tables, model=x)
    expect_equal(names(chk), "t1")
    expect_equal(chk$t1$file1, paste("Bucket paths in t1.file1 do not exist:", file.path(bucket, "foo")))
    
    # check submissions dir
    bucket <- "gs://fc-78a7b775-81f3-4c54-a9fb-da182178a827/"
    dat <- tibble(t1_id=1:2,
                  file1=paste0(bucket, c("HapMap_subject_table.tsv", 
                     "submissions/48cd646d-2e8f-4a5a-8f37-0e9a98c1b6da/genotype_report/ae1df9d6-424d-422c-bd42-c875be6133fd/call-results/cacheCopy/results.log")))
    chk <- check_bucket_paths(tables=list(t1=dat), model=x)
    expect_null(chk$t1$file1)
})
