# library(argparser)
# 
# argp <- argparser("report")
# argp <- add_argument(argp, "--tables", nargs=Inf, help="tsv files with data tables")
# argp <- add_argument(argp, "--model", help="json file with data model")
# argp <- add_argument(argp, "--out_prefix", help="output prefix")
# argv <- parse_args(argp)


model_file <- system.file("extdata", "data_model.json", package="AnvilDataModels")

table_names <- c("subject", "phenotype", "sample", "sample_set", "file")
table_files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
names(table_files) <- table_names

#params <- list(tables=table_files, model=model_file)
argv <- list(out_prefix="test", tables=table_files, model=model_file)

pass <- custom_render_markdown("data_model_report", argv$out_prefix, 
                               parameters=argv[c("tables", "model")])


# check set foreign keys - expect failure
model_file <- system.file("extdata", "data_model_set_fk.json", package="AnvilDataModels")

table_names <- c("sample", "sample_set", "file_multi")
table_files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
names(table_files) <- table_names

argv <- list(out_prefix="test", tables=table_files, model=model_file)

pass <- custom_render_markdown("data_model_report", argv$out_prefix, 
                               parameters=argv[c("tables", "model")])


# create some errors to report
model_file <- system.file("extdata", "data_model.json", package="AnvilDataModels")
table_names <- c("subject", "phenotype", "sample", "sample_set", "file")
table_files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
names(table_files) <- table_names
tables <- read_data_tables(table_files)
names(tables)[5] <- "notfile"
tables$sample$tissue_source <- NULL
tables$sample$hat <- "a"
tables$sample$shoe <- "b"
tables$sample$age_at_sample_collection <- "a"
tables$subject$reported_sex <- sample(LETTERS, nrow(tables$subject), replace=TRUE)
tables$subject$consent_code[1:5] <- NA
tables$sample$sample_id[1] <- tables$sample$sample_id[2]
tables$sample$sample_id[2] <- NA
tables$sample$date_of_sample_processing[1] <- "a"
tables$sample$subject_id[2:3] <- NA
tables$subject <- filter(tables$subject, subject_id != "subject1")
tables$phenotype$visit_id <- NULL
tables$sample_set$sample_id <- NULL
tables$subject$subject_id[1] <- "subject2@"
tables$phenotype$height[1:3] <- c(-1, -2, 500)

table_files <- sapply(tables, function(t) {
    tmp <- tempfile()
    readr::write_tsv(t, tmp)
    return(tmp)
})

#params <- list(tables=table_files, model=model_file)
argv <- list(out_prefix="test", tables=table_files, model=model_file)

pass <- custom_render_markdown("data_model_report", argv$out_prefix, 
                               parameters=argv[c("tables", "model")])

unlink(table_files)


# check google bucket files
model_file <- system.file("extdata", "data_model_files.json", package="AnvilDataModels")
bucket <- "gs://fc-efda2373-416d-45db-bde6-b3ad08bf9d79"
dat <- tibble(t1_id=1:2,
              file1=file.path(bucket, c("file.tsv", "foo")))
tmp <- tempfile()
readr::write_tsv(dat, tmp)
table_files <- c(t1=tmp)

#params <- list(tables=table_files, model=model_file)
argv <- list(out_prefix="test", tables=table_files, model=model_file)

pass <- custom_render_markdown("data_model_report", argv$out_prefix, 
                               parameters=argv[c("tables", "model")])
unlink(table_files)
