# library(argparser)
# 
# argp <- argparser("report")
# argp <- add_argument(argp, "--tables", nargs=Inf, help="tsv files with data tables")
# argp <- add_argument(argp, "--model", help="tsv file with data model")
# argp <- add_argument(argp, "--out_prefix", help="output prefix")
# argv <- parse_args(argp)


model_file <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")

table_names <- c("subject", "phenotype", "sample", "sample_set", "file")
table_files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
names(table_files) <- table_names

#params <- list(tables=table_files, model=model_file)
argv <- list(out_prefix="test", tables=table_files, model=model_file)

custom_render_markdown("data_model_report", argv$out_prefix, 
                       parameters=argv[c("tables", "model")])


# create some errors to report
tables <- read_data_tables(table_files)
names(tables)[5] <- "notfile"
tables$sample$tissue_source <- NULL
tables$sample$hat <- "a"
tables$sample$shoe <- "b"
tables$sample$age_at_sample_collection <- "a"
tables$subject$reported_sex <- "A"
tables$sample$sample_id[1] <- tables$sample$sample_id[2]
tables$subject <- filter(tables$subject, subject_id != "subject1")

table_files <- sapply(tables, function(t) {
    tmp <- tempfile()
    readr::write_tsv(t, tmp)
    return(tmp)
})

#params <- list(tables=table_files, model=model_file)
argv <- list(out_prefix="test", tables=table_files, model=model_file)

custom_render_markdown("data_model_report", argv$out_prefix, 
                       parameters=argv[c("tables", "model")])