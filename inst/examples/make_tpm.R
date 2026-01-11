# Load required data
counts <- read.table("inst/examples/data/expr_matrix.counts.txt", header = TRUE, row.names = 1, sep = "\t", check.names = FALSE)
gene_meta <- read.csv("inst/examples/data/gene_metadata.csv", row.names = 1)

# Ensure gene order matches
gene_lengths <- gene_meta[rownames(counts), "length"]

# Calculate RPK (Reads Per Kilobase)
rpk <- sweep(counts, 1, gene_lengths / 1000, "/")

# Calculate per-sample scaling factor (sum of RPKs)
scaling_factors <- colSums(rpk)

# Calculate TPM
tpm <- sweep(rpk, 2, scaling_factors / 1e6, "/")

# Write output
write.table(round(tpm, 2), file = "inst/examples/data/test.tpm.txt", sep = "\t", quote = FALSE, col.names = NA)