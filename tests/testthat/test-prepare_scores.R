library(testthat)
library(GXPAinterface)

test_that("process_gxpa_scores works correctly", {
  # Setup mock data and directories
  test_dir <- withr::local_tempdir()
  input_dir <- file.path(test_dir, "input")
  output_dir <- file.path(test_dir, "output")
  dir.create(input_dir)
  dir.create(output_dir)

  deg_file <- "test_deg.txt"
  deg_data <- data.frame(
    gene_id = c("G1", "G2", "G3"),
    logFC = c(2, -1.5, 0.5),
    FDR = c(0.01, 0.04, 0.2),
    stringsAsFactors = FALSE
  )
  # Assume first column is gene_id (as handled in process_gxpa_scores)
  utils::write.table(deg_data, file = file.path(input_dir, deg_file), sep = "\t", row.names = FALSE)

  deg_meta <- data.frame(
    score_file_name = deg_file,
    name = "TestComp",
    description = "Test Description",
    logfc_column = "logFC",
    fdr_column = "FDR",
    group1 = "A",
    group2 = "B",
    stringsAsFactors = FALSE
  )

  gene_id_map <- data.frame(
    gene_id = c("G1", "G2", "G3"),
    gene_symbol = c("Sym1", "Sym2", "Sym3"),
    stringsAsFactors = FALSE
  )

  # Run function
  score_info <- process_gxpa_scores(deg_meta, gene_id_map, input_dir, output_dir)

  # Check score_info output
  expect_s3_class(score_info, "data.frame")
  expect_equal(nrow(score_info), 2)
  expect_equal(score_info$score_type, c("logFC", "FDR"))
  
  # Check written file
  expect_true(file.exists(file.path(output_dir, deg_file)))
  written_data <- utils::read.delim(file.path(output_dir, deg_file), row.names = 1)
  expect_equal(rownames(written_data), c("Sym1", "Sym2", "Sym3"))
  expect_true("logFC_rank" %in% colnames(written_data))
  expect_true("FDR_rank" %in% colnames(written_data))
  
  # Check ranks
  expect_equal(written_data$logFC_rank, c(1, 2, 3)) # rank(-abs(2, -1.5, 0.5)) -> rank(-2, -1.5, -0.5) -> 1, 2, 3
  expect_equal(written_data$FDR_rank, c(1, 2, 3)) # rank(0.01, 0.04, 0.2) -> 1, 2, 3
})

test_that("process_gxpa_scores handles empty deg_meta", {
  score_info <- process_gxpa_scores(data.frame(), data.frame(), "in", "out")
  expect_equal(nrow(score_info), 0)
  expect_s3_class(score_info, "data.frame")
})

test_that("process_gxpa_scores warns when many IDs missing", {
  test_dir <- withr::local_tempdir()
  input_dir <- file.path(test_dir, "input")
  output_dir <- file.path(test_dir, "output")
  dir.create(input_dir)
  dir.create(output_dir)

  deg_file <- "low_overlap.txt"
  # Score file only has G1
  deg_data <- data.frame(
    id = "G1",
    LFC = 1,
    FDR = 0.05
  )
  utils::write.table(deg_data, file = file.path(input_dir, deg_file), sep = "\t", row.names = FALSE)

  deg_meta <- data.frame(
    score_file_name = deg_file,
    name = "Test",
    description = "test",
    logfc_column = "LFC",
    fdr_column = "FDR",
    group1 = "A",
    group2 = "B",
    stringsAsFactors = FALSE
  )
  
  # Map contains G1 to G10
  gene_id_map <- data.frame(
    gene_id = paste0("G", 1:10),
    gene_symbol = paste0("Sym", 1:10),
    stringsAsFactors = FALSE
  )

  expect_warning(
    process_gxpa_scores(deg_meta, gene_id_map, input_dir, output_dir),
    "expected IDs found"
  )
})

test_that("write_mean_sd_scores works correctly", {
  test_dir <- withr::local_tempdir()
  
  tpm_mat <- matrix(
    c(1, 2, 3, 
      10, 20, 30), 
    nrow = 2, byrow = TRUE,
    dimnames = list(c("G1", "G2"), c("S1", "S2", "S3"))
  )
  
  score_info <- write_mean_sd_scores(tpm_mat, test_dir)
  
  expect_equal(nrow(score_info), 2)
  expect_true(file.exists(file.path(test_dir, "expr_data.mean.txt")))
  expect_true(file.exists(file.path(test_dir, "expr_data.stdev.txt")))
  
  mean_data <- utils::read.delim(file.path(test_dir, "expr_data.mean.txt"), row.names = 1, check.names = FALSE)
  expect_equal(nrow(mean_data), 2)
  expect_equal(mean_data["G2", "mean"], 20)
  expect_equal(mean_data["G1", "mean"], 2)
  
  sd_data <- utils::read.delim(file.path(test_dir, "expr_data.stdev.txt"), row.names = 1, check.names = FALSE)
  expect_equal(nrow(sd_data), 2)
  # G2: 10, 20, 30 -> mean 20, sd 10
  # G1: 1, 2, 3 -> mean 2, sd 1
  expect_equal(sd_data["G2", "stdev"], 10)
  expect_equal(sd_data["G1", "stdev"], 1)
})
