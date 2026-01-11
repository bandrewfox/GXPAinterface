library(testthat)
library(GXPAinterface)
library(SummarizedExperiment)

test_that("filter_se_samples works", {
  counts <- matrix(1:12, ncol = 3, dimnames = list(c("g1", "g2", "g3", "g4"), c("s1", "s2", "s3")))
  col_data <- data.frame(group = c("A", "A", "B"), batch = c(1, 2, 1), row.names = c("s1", "s2", "s3"))
  se <- SummarizedExperiment(assays = list(counts = counts), colData = col_data)
  
  # Remove s3 (group B)
  se_filt <- filter_se_samples(se, list(group = "B"))
  expect_equal(ncol(se_filt), 2)
  expect_equal(colnames(se_filt), c("s1", "s2"))
  
  # Remove s2 (batch 2)
  se_filt2 <- filter_se_samples(se, list(batch = 2))
  expect_equal(ncol(se_filt2), 2)
  expect_equal(colnames(se_filt2), c("s1", "s3"))

  # No removal
  se_none <- filter_se_samples(se, list(group = "C"))
  expect_equal(ncol(se_none), 3)
})

test_that("assemble_gxpa_se works", {
  counts_df <- data.frame(gene_id = c("g1", "g2", "g3"), s1 = 1:3, s2 = 4:6)
  tpm_df <- data.frame(gene_id = c("g1", "g2", "g3"), s1 = 11:13, s2 = 14:16)
  sample_meta <- data.frame(sample_id = c("s1", "s2"), group = c("A", "B"))
  gene_meta <- data.frame(gene_id = c("g1", "g2", "g3"), symbol = c("G1", "G2", "G3"))
  
  se <- assemble_gxpa_se(
    counts_df = counts_df, 
    tpm_df = tpm_df, 
    sample_meta = sample_meta, 
    sample_id_col = "sample_id", 
    gene_meta = gene_meta, 
    gene_id_col = "gene_id"
  )
  
  expect_s4_class(se, "SummarizedExperiment")
  expect_equal(assayNames(se), c("tpm", "counts"))
  expect_equal(nrow(se), 3)
  expect_equal(ncol(se), 2)
  expect_equal(rownames(se), c("g1", "g2", "g3"))
  expect_equal(colnames(se), c("s1", "s2"))
  expect_equal(rowData(se)$symbol, c("G1", "G2", "G3"))
})

test_that("assemble_gxpa_se errors on missing cols", {
  sample_meta <- data.frame(sample_id = c("s1", "s2"), group = c("A", "B"))
  gene_meta <- data.frame(gene_id = c("g1", "g2", "g3"), symbol = c("G1", "G2", "G3"))
  
  expect_error(
    assemble_gxpa_se(NULL, NULL, sample_meta, "sample_id", gene_meta, "gene_id"),
    "At least one of counts_df or tpm_df must be provided"
  )
  
  expect_error(
    assemble_gxpa_se(data.frame(g=1, s=1), NULL, sample_meta, "WRONG", gene_meta, "gene_id"),
    "sample_id_col 'WRONG' not found in sample_meta"
  )
})

test_that("extract_se_constant_metadata works", {
  col_data <- data.frame(
    sample = c("s1", "s2"),
    study = c("Study1", "Study1"),
    group = c("A", "B"),
    row.names = c("s1", "s2")
  )
  se <- SummarizedExperiment(assays = list(counts = matrix(1:4, 2)), colData = col_data)
  
  res <- extract_se_constant_metadata(se, remove_from_se = TRUE)
  
  expect_equal(res$constant_meta$study, "Study1")
  expect_false("study" %in% colnames(colData(res$se)))
  expect_true("group" %in% colnames(colData(res$se)))
})

test_that("deduplicate_se_by_symbol works", {
  # p1 and p1.1 both map to G1. G1 should take p1.1 because 10 > 1.
  # p2 and p2.1 both map to G2. G2 should take p2.1 because 5 > 2.
  counts <- matrix(c(1, 10, 2, 5), nrow = 4, dimnames = list(c("p1", "p1.1", "p2", "p2.1"), "s1"))
  row_data <- data.frame(symbol = c("G1", "G1", "G2", "G2"), row.names = c("p1", "p1.1", "p2", "p2.1"))
  se <- SummarizedExperiment(assays = list(tpm = counts), rowData = row_data)
  
  se_dedup <- deduplicate_se_by_symbol(se, "symbol")
  
  expect_equal(nrow(se_dedup), 2)
  expect_equal(rownames(se_dedup), c("G1", "G2"))
  expect_equal(as.numeric(assay(se_dedup, "tpm")), c(10, 5))
})

test_that("deduplicate_se_by_symbol errors", {
  se <- SummarizedExperiment(assays = list(tpm = matrix(1:4, 2)), rowData = data.frame(sym=c("A","B")))
  expect_error(deduplicate_se_by_symbol(se, "sym", primary_assay = "MISSING"), "Primary assay 'MISSING' not found")
  expect_error(deduplicate_se_by_symbol(se, "WRONG"), "Symbol column 'WRONG' not found")
})

test_that("write_se_to_gxpa works", {
  test_dir <- withr::local_tempdir()
  se <- SummarizedExperiment(
    assays = list(tpm = matrix(1:4, 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))),
    colData = data.frame(group = c("A", "B"), row.names = c("s1", "s2"))
  )
  
  # Run without checks to avoid dependency on check_files_for_GXPA logic/path issues in test
  write_se_to_gxpa(se, test_dir, run_checks = FALSE)
  
  expect_true(file.exists(file.path(test_dir, "expr.samples.csv")))
  expect_true(file.exists(file.path(test_dir, "expr.expr.txt")))
})
