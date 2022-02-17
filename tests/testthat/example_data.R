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
    age_at_sample_collection = round(rnorm(n+1, 40, 20)),
    date_of_sample_processing = c("2000-01-02", rep("2000-01-01", n))
)

sample_set <- tibble(
    sample_set_id = c(rep("set1", 3), rep("set2", 5)),
    sample_id = c(sample$sample_id[2:4], sample$sample_id[3:7])
)

nf <- (n+1)*2
filename <- paste(sample$sample_id, sapply(1:(n+1), function(x) paste0(sample(letters, 6, replace=TRUE), collapse="")), sep="_")
file <- tibble(
    md5 = sapply(1:nf, function(x) paste0(sample(c(letters,0:9), 32, replace=TRUE), collapse="")),
    sample_id = rep(sample$sample_id, each=2),
    filename = paste0(rep(filename, each=2), rep(c(".bam", ".bai"), length(filename))),
    file_timestamp = rep("2001-01-01 12:00:00", nf)
)

write_tsv(subject, "inst/extdata/subject.tsv")
write_tsv(phenotype, "inst/extdata/phenotype.tsv")
write_tsv(sample, "inst/extdata/sample.tsv")
write_tsv(sample_set, "inst/extdata/sample_set.tsv")
write_tsv(file, "inst/extdata/file.tsv")
