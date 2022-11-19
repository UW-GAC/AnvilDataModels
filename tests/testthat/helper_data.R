
.tables <- function() {
    table_names <- c("subject", "phenotype", "sample", "sample_set", "file")
    files <- system.file("extdata", paste0(table_names, ".tsv"), package="AnvilDataModels")
    read_data_tables(files, table_names=table_names, quiet=TRUE)
}

.model <- function() {
    json <- system.file("extdata", "data_model.json", package="AnvilDataModels")
    json_to_dm(json)
}
