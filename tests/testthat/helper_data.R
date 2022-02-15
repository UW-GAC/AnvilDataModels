
.tables <- function() {
    table_names <- c("subject", "phenotype", "sample", "file")
    files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
    read_data_tables(files, table_names=table_names, quiet=TRUE)
}

.model <- function() {
    tsv <- system.file("extdata", "data_model.tsv", package="AnvilDataModels")
    tsv_to_dm(tsv)
}
