#' Process and Export Score Files for GXPA
#'
#' Filters and ranks DEG score files, then writes them to the output directory
#' and generates the `expr.score_info.csv` file.
#'
#' @param deg_meta A data frame containing score file metadata. Required columns:
#' `score_file_name`, `name`, `description`, `logfc_column`, `fdr_column`, `group1`, `group2`.
#' @param gene_id_map A data frame mapping original gene IDs to symbols. Must contain
#' `gene_id` (matching original score files) and `gene_symbol` (final output names).
#' @param input_dir Directory containing the raw score files.
#' @param output_dir Directory where processed score files will be written.
#' @param score_thresholds A list with `logFC` and `FDR` elements for calculating rank thresholds.
#' Default `list(logFC = 1, FDR = 0.05)`.
#'
#' @return A data frame containing the generated score info records.
#' @export
process_gxpa_scores <- function(deg_meta,
                                gene_id_map,
                                input_dir,
                                output_dir,
                                score_thresholds = list(logFC = 1, FDR = 0.05)) {
  if (nrow(deg_meta) == 0) return(data.frame())
  
  score_info_file <- data.frame()
  
  for (i in seq_len(nrow(deg_meta))) {
    score_file_path <- file.path(input_dir, deg_meta$score_file_name[i])
    if (!file.exists(score_file_path)) {
      warning("Score file does not exist: ", score_file_path, ". Skipping.")
      next
    }

    score_data <- utils::read.delim(score_file_path, stringsAsFactors = FALSE, check.names = FALSE)
    # Assume first column is the gene ID
    colnames(score_data)[1] <- "gene_id"

    # Filter to genes we are keeping
    gene_ids_to_keep <- gene_id_map$gene_id
    frac_found <- sum(gene_ids_to_keep %in% score_data$gene_id) / length(gene_ids_to_keep)
    if (frac_found < 0.2) {
      warning(paste0("Only ", round(frac_found * 100, 1), "% of expected IDs found in ", deg_meta$score_file_name[i]))
    }

    score_data <- score_data %>% dplyr::filter(.data$gene_id %in% gene_ids_to_keep)

    # Ranks
    logfc_col <- deg_meta$logfc_column[i]
    fdr_col <- deg_meta$fdr_column[i]

    score_data <- score_data %>%
      dplyr::mutate(
        logFC_rank = rank(-abs(.data[[logfc_col]]), ties.method = "first", na.last = "keep"),
        FDR_rank   = rank(.data[[fdr_col]], ties.method = "first", na.last = "keep")
      )

    # Join to get symbols and set as rownames
    score_data <- score_data %>%
      dplyr::left_join(gene_id_map, by = "gene_id")
    
    # Check for missing symbols after join
    if (any(is.na(score_data$gene_symbol))) {
        warning("Some gene_ids could not be mapped to gene_symbols in ", deg_meta$score_file_name[i])
        score_data <- score_data[!is.na(score_data$gene_symbol), ]
    }
    
    rownames(score_data) <- score_data$gene_symbol
    score_data <- score_data %>% dplyr::select(-"gene_symbol")

    # Get indices for score info
    logfc_col_idx <- which(colnames(score_data) == logfc_col)
    fdr_col_idx <- which(colnames(score_data) == fdr_col)
    logfc_rank_col_idx <- which(colnames(score_data) == "logFC_rank")
    fdr_rank_col_idx <- which(colnames(score_data) == "FDR_rank")

    # Info rows
    logfc_info_row <- data.frame(
      name = paste0(deg_meta$name[i], "-logFC"),
      filename = deg_meta$score_file_name[i],
      description = paste0(deg_meta$description[i], " (logFC)"),
      score_type = "logFC",
      rank_threshold = min(sum(abs(score_data[[logfc_col]]) >= score_thresholds$logFC, na.rm = TRUE), 5),
      score_col = logfc_col_idx,
      rank_col = logfc_rank_col_idx,
      group1 = deg_meta$group1[i],
      group2 = deg_meta$group2[i],
      stringsAsFactors = FALSE
    )

    fdr_info_row <- data.frame(
      name = paste0(deg_meta$name[i], "-FDR"),
      filename = deg_meta$score_file_name[i],
      description = paste0(deg_meta$description[i], " (FDR)"),
      score_type = "FDR",
      rank_threshold = min(sum(score_data[[fdr_col]] <= score_thresholds$FDR, na.rm = TRUE), 5),
      score_col = fdr_col_idx,
      rank_col = fdr_rank_col_idx,
      group1 = deg_meta$group1[i],
      group2 = deg_meta$group2[i],
      stringsAsFactors = FALSE
    )

    score_info_file <- rbind(score_info_file, logfc_info_row, fdr_info_row)

    # Write out
    new_file_path <- file.path(output_dir, deg_meta$score_file_name[i])
    utils::write.table(score_data, file = new_file_path, sep = "\t", row.names = TRUE, quote = FALSE)
  }
  
  return(score_info_file)
}

#' Calculate and Write Mean and SD Score Files
#'
#' @param tpm_mat Expression matrix (gene names as rownames).
#' @param output_dir Directory to write the files.
#' @param prefix Prefix for the filenames. Default "expr_data".
#'
#' @return A data frame with score info records for mean and SD.
#' @export
write_mean_sd_scores <- function(tpm_mat, output_dir, prefix = "expr_data") {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  mean_file <- paste0(prefix, ".mean.txt")
  sd_file <- paste0(prefix, ".stdev.txt")

  # Mean
  expr_means <- data.frame(
    mean = rowMeans(tpm_mat, na.rm = TRUE),
    stringsAsFactors = FALSE
  ) %>% 
    dplyr::arrange(dplyr::desc(.data$mean)) %>% 
    dplyr::mutate(rank = seq_len(dplyr::n()))
  
  expr_means$mean <- round(expr_means$mean, 4)
  utils::write.table(expr_means, file = file.path(output_dir, mean_file), sep = "\t", row.names = TRUE, quote = FALSE)

  # SD
  expr_sd <- data.frame(
    stdev = apply(tpm_mat, 1, stats::sd, na.rm = TRUE),
    stringsAsFactors = FALSE
  ) %>% 
    dplyr::arrange(dplyr::desc(.data$stdev)) %>% 
    dplyr::mutate(rank = seq_len(dplyr::n()))
  
  expr_sd$stdev <- round(expr_sd$stdev, 4)
  utils::write.table(expr_sd, file = file.path(output_dir, sd_file), sep = "\t", row.names = TRUE, quote = FALSE)

  score_info_rows <- data.frame(
    name = c("mean", "stdev"),
    filename = c(mean_file, sd_file),
    description = c("mean across all samples", "stdev across all samples"),
    score_type = c("mean", "stdev"),
    rank_threshold = 200,
    score_col = 1,
    rank_col = 2,
    group1 = "all",
    group2 = "",
    stringsAsFactors = FALSE
  )
  
  return(score_info_rows)
}
