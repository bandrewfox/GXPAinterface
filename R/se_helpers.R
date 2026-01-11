#' Filter Samples from SummarizedExperiment
#'
#' @param se A SummarizedExperiment object.
#' @param remove_list A named list where names are column names in `colData(se)`
#' and values are vectors of values to remove.
#'
#' @return A filtered SummarizedExperiment object.
#' @export
filter_se_samples <- function(se, remove_list) {
  if (is.null(remove_list) || length(remove_list) == 0) return(se)

  tmp <- as.data.frame(SummarizedExperiment::colData(se))
  remove_ids <- purrr::imap(remove_list, function(x, idx) {
    if (idx %in% colnames(tmp)) {
      rownames(tmp)[tmp[[idx]] %in% x]
    } else {
      character(0)
    }
  }) %>% unlist()

  if (length(remove_ids) > 0) {
    message("Removing ", length(remove_ids), " samples based on remove_list criteria.")
    se <- se[, !(colnames(se) %in% remove_ids)]
  }
  return(se)
}

#' Assemble SummarizedExperiment for GXPA
#'
#' Takes expression matrices and metadata data frames and assembles them into
#' a SummarizedExperiment, ensuring all samples and genes are aligned.
#'
#' @param counts_df Data frame with counts (first column must be Gene ID).
#' @param tpm_df Data frame with TPM (first column must be Gene ID).
#' @param sample_meta Data frame with sample metadata.
#' @param sample_id_col Column name in `sample_meta` containing sample IDs.
#' @param gene_meta Data frame with gene metadata.
#' @param gene_id_col Column name in `gene_meta` containing gene IDs.
#'
#' @return A SummarizedExperiment object.
#' @export
assemble_gxpa_se <- function(counts_df = NULL,
                             tpm_df = NULL,
                             sample_meta,
                             sample_id_col,
                             gene_meta,
                             gene_id_col) {
  if (is.null(counts_df) && is.null(tpm_df)) stop("At least one of counts_df or tpm_df must be provided.")

  # 1. Prepare Gene Metadata
  row_df <- gene_meta
  if (!gene_id_col %in% colnames(row_df)) stop("gene_id_col '", gene_id_col, "' not found in gene_meta.")
  rownames(row_df) <- row_df[[gene_id_col]]
  
  # Ensure unique_gene_id column exists
  if (!"unique_gene_id" %in% colnames(row_df)) {
    row_df$unique_gene_id <- row_df[[gene_id_col]]
  }

  # 2. Prepare Sample Metadata
  col_df <- sample_meta
  if (!sample_id_col %in% colnames(col_df)) stop("sample_id_col '", sample_id_col, "' not found in sample_meta.")
  rownames(col_df) <- col_df[[sample_id_col]]

  # 3. Process Expression Matrices
  process_expr <- function(df) {
    if (is.null(df)) return(NULL)
    # Assume first column is gene_id
    g_col_name <- colnames(df)[1]
    df_clean <- df %>% dplyr::filter(.data[[g_col_name]] %in% rownames(row_df))
    rownames(df_clean) <- df_clean[[g_col_name]]
    df_clean <- df_clean %>% dplyr::select(-dplyr::all_of(g_col_name))
    return(as.matrix(df_clean))
  }

  counts_mat <- process_expr(counts_df)
  tpm_mat <- process_expr(tpm_df)

  # 4. Alignment
  primary_mat <- if (!is.null(tpm_mat)) tpm_mat else counts_mat
  
  # Filter row_df to match expression
  row_df <- row_df[match(rownames(primary_mat), rownames(row_df)), , drop = FALSE]
  
  # Align samples
  sample_names_meta <- rownames(col_df)
  sample_names_expr <- colnames(primary_mat)
  
  if (!all(sample_names_meta %in% sample_names_expr)) {
      missing <- setdiff(sample_names_meta, sample_names_expr)
      message("Warning: Sample metadata contains samples not found in expression matrices (", 
              length(missing), " missing). Filtering colData.")
      col_df <- col_df[sample_names_meta %in% sample_names_expr, , drop = FALSE]
      sample_names_meta <- rownames(col_df)
  }
  
  # Subset and reorder matrices to match col_df
  if (!is.null(counts_mat)) counts_mat <- counts_mat[, sample_names_meta, drop = FALSE]
  if (!is.null(tpm_mat)) tpm_mat <- tpm_mat[, sample_names_meta, drop = FALSE]

  assays_list <- list()
  if (!is.null(tpm_mat)) assays_list$tpm <- tpm_mat
  if (!is.null(counts_mat)) assays_list$counts <- counts_mat

  se <- SummarizedExperiment::SummarizedExperiment(assays = assays_list, rowData = row_df, colData = col_df)
  return(se)
}

#' Extract Constant Metadata Columns from SummarizedExperiment
#'
#' Identified columns in `colData(se)` that have only one unique value across
#' all samples and returns them as a list, optionally removing them from the SE.
#'
#' @param se A SummarizedExperiment object.
#' @param remove_from_se Logical, if TRUE removes the constant columns from the SE.
#'
#' @return A list with two elements: `se` (the modified or original SE) and
#' `constant_meta` (a named list of the constant values).
#' @export
extract_se_constant_metadata <- function(se, remove_from_se = TRUE) {
  tmp <- as.data.frame(SummarizedExperiment::colData(se))
  if (ncol(tmp) == 0) return(list(se = se, constant_meta = list()))

  cols_to_move <- as.logical(purrr::map_dbl(tmp, ~ length(unique(.x))) == 1)
  
  if (any(cols_to_move)) {
    constant_df <- tmp[1, cols_to_move, drop = FALSE]
    constant_meta <- as.list(constant_df)
    
    if (remove_from_se) {
      SummarizedExperiment::colData(se) <- SummarizedExperiment::colData(se)[, !cols_to_move, drop = FALSE]
    }
  } else {
    constant_meta <- list()
  }
  
  return(list(se = se, constant_meta = constant_meta))
}

#' Deduplicate SummarizedExperiment by Gene Symbol
#'
#' Keeps the row with the highest mean expression per gene symbol.
#'
#' @param se A SummarizedExperiment object.
#' @param symbol_col Character, the column name in `rowData(se)` containing gene symbols.
#' @param primary_assay Character, the assay name to use for calculating mean expression. Default "tpm".
#'
#' @return A deduplicated SummarizedExperiment object where rownames are gene symbols.
#' @export
deduplicate_se_by_symbol <- function(se, symbol_col, primary_assay = "tpm") {
  if (!primary_assay %in% SummarizedExperiment::assayNames(se)) {
    stop("Primary assay '", primary_assay, "' not found in SE. Available assays: ", 
         paste(SummarizedExperiment::assayNames(se), collapse = ", "))
  }

  row_meta <- as.data.frame(SummarizedExperiment::rowData(se))
  if (!symbol_col %in% colnames(row_meta)) {
    stop("Symbol column '", symbol_col, "' not found in rowData(se).")
  }

  # Add unique_gene_id if not present for tracking
  if (!"unique_gene_id" %in% colnames(row_meta)) {
    row_meta$unique_gene_id <- rownames(row_meta)
  }

  row_meta$gene_symbol_for_dedup <- row_meta[[symbol_col]]
  row_means <- rowMeans(SummarizedExperiment::assay(se, primary_assay), na.rm = TRUE)
  row_meta$mean_for_dedup <- row_means

  best_ids <- row_meta %>%
    dplyr::arrange(dplyr::desc(mean_for_dedup)) %>%
    dplyr::distinct(gene_symbol_for_dedup, .keep_all = TRUE) %>%
    dplyr::pull(unique_gene_id)

  se_dedup <- se[best_ids, ]
  rownames(se_dedup) <- SummarizedExperiment::rowData(se_dedup)[[symbol_col]]
  
  return(se_dedup)
}

#' Write SummarizedExperiment to GXPA files
#'
#' Exports a SummarizedExperiment object to the flat files expected by GXPA.
#'
#' @param se A SummarizedExperiment object.
#' @param output_dir Path to the directory where files will be written.
#' @param tpm_assay Name of the TPM assay. Default "tpm".
#' @param counts_assay Name of the counts assay. Default "counts".
#' @param run_checks Logical, whether to run `check_files_for_GXPA` after writing. Default TRUE.
#'
#' @return Invisible NULL.
#' @export
write_se_to_gxpa <- function(se, output_dir, tpm_assay = "tpm", counts_assay = "counts", run_checks = TRUE) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # 1. Samples Metadata
  samples_df <- as.data.frame(SummarizedExperiment::colData(se))
  # Ensure _id is the first column
  samples_df <- data.frame(`_id` = colnames(se), samples_df, check.names = FALSE)
  samples_path <- file.path(output_dir, "expr.samples.csv")
  utils::write.csv(samples_df, file = samples_path, row.names = FALSE, quote = FALSE)

  # 2. TPM Assay
  expr_path <- NULL
  if (tpm_assay %in% SummarizedExperiment::assayNames(se)) {
    tpm_mat <- SummarizedExperiment::assay(se, tpm_assay)
    expr_path <- file.path(output_dir, "expr.expr.txt")
    utils::write.table(tpm_mat, file = expr_path, sep = "\t", row.names = TRUE, quote = FALSE)
  }

  # 3. Counts Assay
  if (counts_assay %in% SummarizedExperiment::assayNames(se)) {
    counts_mat <- SummarizedExperiment::assay(se, counts_assay)
    utils::write.table(counts_mat, file = file.path(output_dir, "expr.expr_counts.txt"), 
                       sep = "\t", row.names = TRUE, quote = FALSE)
  }

  if (run_checks && !is.null(expr_path)) {
    check_files_for_GXPA(expr_file = expr_path, samples_file = samples_path)
  }

  invisible(NULL)
}
