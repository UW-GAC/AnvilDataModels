library(tibble)
library(readr)

set.seed(42)
n <- 10

subject <- tibble(
    subject_id = paste0("subject", 1:n),
    consent_code = rep("GRU", n),
    study_nickname = rep("study", n),
    dbgap_submission = rep(FALSE, n),
    reported_sex = sample(c("F", "M", "X"), n, replace=TRUE)
)

phenotype <- tibble(
    subject_id = rep(subject$subject_id, each=2),
    visit_id = rep(1:2, n),
    height = rep(rnorm(n, 170, 10), each=2),
    weight = rnorm(n*2, 75, 5)
)

sample <- tibble(
    sample_id = paste0("sample", c("1a", 1:n)),
    subject_id = c(subject$subject_id[1], subject$subject_id),
    tissue_source = "blood",
    age_at_sample_collection = round(runif(n+1, 20, 80)),
    date_of_sample_processing = c("2000-01-02", rep("2000-01-01", n))
)

sample_set <- tibble(
    sample_set_id = c(rep("set1", 3), rep("set2", 5)),
    sample_id = c(sample$sample_id[2:4], sample$sample_id[3:7])
)

rand_string <- function(x) {
    paste0(sample(c(letters,0:9), x, replace=TRUE), collapse="")
}

nf <- (n+1)*2
filename <- paste(sample$sample_id, sapply(1:(n+1), function(x) rand_string(6)), sep="_")
file <- tibble(
    md5 = sapply(1:nf, function(x) rand_string(32)),
    sample_id = rep(sample$sample_id, each=2),
    filename = paste0(rep(filename, each=2), rep(c(".bam", ".bai"), length(filename))),
    file_timestamp = rep("2001-01-01 12:00:00", nf)
)

write_tsv(subject, "inst/extdata/subject.tsv")
write_tsv(phenotype, "inst/extdata/phenotype.tsv")
write_tsv(sample, "inst/extdata/sample.tsv")
write_tsv(sample_set, "inst/extdata/sample_set.tsv")
write_tsv(file, "inst/extdata/file.tsv")


analysis1 <- tibble(
    analysis_type = "GWAS",
    outcome_type = "quantitative",
    outcome = "SBP"
)

analysis2 <- tibble(
    analysis_type = "GWAS",
    outcome_type = "binary",
    outcome = "hypertension"
)

set.seed(1)
file1 <- tibble(
    md5 = sapply(1:3, function(x) rand_string(32)),
    filename = paste0(sapply(1:3, function(x) rand_string(6)), ".tsv")
)

set.seed(2)
file2 <- tibble(
    md5 = sapply(1:3, function(x) rand_string(32)),
    filename = paste0(sapply(1:3, function(x) rand_string(6)), ".tsv")
)

write_tsv(analysis1, "inst/extdata/analysis1.tsv")
write_tsv(analysis2, "inst/extdata/analysis2.tsv")
write_tsv(file1, "inst/extdata/analysis_file1.tsv")
write_tsv(file2, "inst/extdata/analysis_file2.tsv")


sets <- c(unique(sample_set$sample_set_id), "missing_set")
nf <- length(sets)
file_multi <- tibble(
    md5 = sapply(1:3, function(x) rand_string(32)),
    sample_set_id = sets,
    filename = paste0(sets, ".vcf")
)
write_tsv(file_multi, "inst/extdata/file_multi.tsv")

