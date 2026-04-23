# scripts/03_factor_analysis_PCA.R
# Step 3: Principal Component Analysis (PCA)
# AI Impact on Jobs 2030 Dataset

# Clear environment
rm(list = ls())

# Set working directory
setwd("C:/Users/zahra/OneDrive/Desktop/Project_R")

# Load required libraries
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(corrplot)

cat("\n==========================================")
cat("\nStep 3: Principal Component Analysis (PCA)")
cat("\n==========================================\n")

# Load processed data
cat("\nLoading processed data...\n")
data_final <- readRDS("data/processed_data.rds")

# Display data info
cat("\nData loaded successfully!")
cat("\n   Dimensions:", nrow(data_final), "x", ncol(data_final))

# Select numeric variables for PCA
numeric_vars <- data_final %>% select(where(is.numeric))
cat("\n   Numeric variables for PCA:", ncol(numeric_vars), "\n")

# Display variable names
cat("\nVariables to be analyzed in PCA:\n")
print(names(numeric_vars))

# Check if we have enough variables
if (ncol(numeric_vars) < 2) {
  stop("Not enough numeric variables for PCA! Need at least 2 variables.")
}

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n3.1 PERFORMING PCA")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Perform PCA (no additional scaling since data is already standardized)
cat("Running PCA on", ncol(numeric_vars), "variables...\n")
pca_result <- PCA(numeric_vars, scale.unit = FALSE, graph = FALSE)

cat("\nPCA completed successfully!\n")

# Save PCA result
saveRDS(pca_result, "data/pca_result.rds")
cat("PCA result saved to: data/pca_result.rds\n")

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n3.2 EXPLAINED VARIANCE")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Get eigenvalues and explained variance
eigenvalues <- pca_result$eig

# Create variance table
variance_table <- data.frame(
  Dimension = 1:nrow(eigenvalues),
  Eigenvalue = round(eigenvalues[, 1], 3),
  Variance_Percent = round(eigenvalues[, 2], 2),
  Cumulative_Percent = round(eigenvalues[, 3], 2)
)

cat("\nExplained Variance Table:\n")
print(variance_table)

# Determine number of dimensions to retain
# Using Kaiser criterion (eigenvalue > 1)
n_kaiser <- sum(eigenvalues[, 1] > 1)
cat("\nKaiser criterion (eigenvalue > 1):", n_kaiser, "dimensions to retain\n")

# Using cumulative variance (80% threshold)
n_80percent <- which(eigenvalues[, 3] >= 80)[1]
if(!is.na(n_80percent)) {
  cat("80% cumulative variance:", n_80percent, "dimensions to retain\n")
} else {
  cat("80% cumulative variance not reached - using all dimensions\n")
  n_80percent <- nrow(eigenvalues)
}

# Recommended dimensions for interpretation
n_interpret <- min(n_kaiser, n_80percent, 5)
cat("Recommended dimensions for interpretation:", n_interpret, "\n")

# Scree Plot
cat("\nGenerating Scree Plot...\n")
p_scree <- fviz_eig(pca_result, 
                    addlabels = TRUE, 
                    ylim = c(0, 30),
                    ggtheme = theme_minimal(),
                    title = "Scree Plot - Explained Variance by Dimension")

ggsave("outputs/figures/scree_plot.png", p_scree, width = 10, height = 6)
cat("Scree plot saved to: outputs/figures/scree_plot.png\n")

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n3.3 VARIABLE CONTRIBUTIONS TO DIMENSIONS")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Get variable contributions
var_contrib <- pca_result$var$contrib

# Display top contributing variables for Dim1
cat("\nTOP 10 VARIABLES CONTRIBUTING TO DIMENSION 1:\n")
dim1_contrib <- sort(var_contrib[, 1], decreasing = TRUE)
top_dim1 <- data.frame(
  Variable = names(head(dim1_contrib, 10)),
  Contribution = round(head(dim1_contrib, 10), 2),
  Cum_Percent = round(cumsum(head(dim1_contrib, 10)), 2)
)
print(top_dim1)

# Display top contributing variables for Dim2
cat("\nTOP 10 VARIABLES CONTRIBUTING TO DIMENSION 2:\n")
dim2_contrib <- sort(var_contrib[, 2], decreasing = TRUE)
top_dim2 <- data.frame(
  Variable = names(head(dim2_contrib, 10)),
  Contribution = round(head(dim2_contrib, 10), 2),
  Cum_Percent = round(cumsum(head(dim2_contrib, 10)), 2)
)
print(top_dim2)

# Save contributions to file
sink("outputs/tables/variable_contributions.txt")
cat("VARIABLE CONTRIBUTIONS TO PCA DIMENSIONS\n")
cat("=========================================\n\n")
cat("Dimension 1 - All variables:\n")
print(round(sort(var_contrib[, 1], decreasing = TRUE), 2))
cat("\n\nDimension 2 - All variables:\n")
print(round(sort(var_contrib[, 2], decreasing = TRUE), 2))
cat("\n\nDimension 3 - All variables:\n")
print(round(sort(var_contrib[, 3], decreasing = TRUE), 2))
sink()
cat("Contributions saved to: outputs/tables/variable_contributions.txt\n")

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n3.4 VARIABLE CORRELATION WITH DIMENSIONS (COS2)")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Get variable correlations (cos2)
var_cos2 <- pca_result$var$cos2

cat("\nQuality of representation (cos2) for first 3 dimensions:\n")
cos2_table <- data.frame(
  Variable = rownames(var_cos2),
  Dim1_Cos2 = round(var_cos2[, 1], 3),
  Dim2_Cos2 = round(var_cos2[, 2], 3),
  Dim3_Cos2 = round(var_cos2[, 3], 3),
  Sum_Cos2_123 = round(rowSums(var_cos2[, 1:3]), 3)
) %>% arrange(desc(Sum_Cos2_123))

print(head(cos2_table, 10))

# Identify well-represented variables (cos2 > 0.5 in first 2 dims)
well_represented <- cos2_table %>% 
  filter(Dim1_Cos2 + Dim2_Cos2 > 0.5)

cat("\nVariables well-represented in first 2 dimensions (cos2 > 0.5):\n")
if(nrow(well_represented) > 0) {
  print(well_represented$Variable)
} else {
  cat("   No variables have cos2 > 0.5 - interpretation may require more dimensions\n")
}

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n3.5 VARIABLE FACTOR MAP (CORRELATION CIRCLE)")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Correlation circle (variables on dimensions 1-2)
p_circle <- fviz_pca_var(pca_result,
                         axes = c(1, 2),
                         col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE,
                         title = "Variable Factor Map - Dim1 vs Dim2",
                         xlab = paste0("Dim1 (", variance_table$Variance_Percent[1], "%)"),
                         ylab = paste0("Dim2 (", variance_table$Variance_Percent[2], "%)"))

ggsave("outputs/figures/variable_factor_map_dim1_dim2.png", p_circle, width = 10, height = 8)
cat("Variable factor map (Dim1-Dim2) saved to: outputs/figures/variable_factor_map_dim1_dim2.png\n")

# Correlation circle for dimensions 2-3
p_circle_23 <- fviz_pca_var(pca_result,
                            axes = c(2, 3),
                            col.var = "contrib",
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                            repel = TRUE,
                            title = "Variable Factor Map - Dim2 vs Dim3",
                            xlab = paste0("Dim2 (", variance_table$Variance_Percent[2], "%)"),
                            ylab = paste0("Dim3 (", variance_table$Variance_Percent[3], "%)"))

ggsave("outputs/figures/variable_factor_map_dim2_dim3.png", p_circle_23, width = 10, height = 8)
cat("Variable factor map (Dim2-Dim3) saved to: outputs/figures/variable_factor_map_dim2_dim3.png\n")

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n3.6 INDIVIDUAL CONTRIBUTIONS")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Get individual contributions
ind_contrib <- pca_result$ind$contrib

# Summary of individual contributions
cat("\nIndividual contributions summary:\n")
cat("  Top 5 individuals contributing to Dim1:\n")
top_ind_dim1 <- head(sort(ind_contrib[, 1], decreasing = TRUE), 5)
print(round(top_ind_dim1, 2))

cat("\n  Top 5 individuals contributing to Dim2:\n")
top_ind_dim2 <- head(sort(ind_contrib[, 2], decreasing = TRUE), 5)
print(round(top_ind_dim2, 2))

# Individuals factor map
p_individuals <- fviz_pca_ind(pca_result,
                              axes = c(1, 2),
                              geom = "point",
                              col.ind = "cos2",
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                              repel = TRUE,
                              title = "Individuals Factor Map - Dim1 vs Dim2",
                              xlab = paste0("Dim1 (", variance_table$Variance_Percent[1], "%)"),
                              ylab = paste0("Dim2 (", variance_table$Variance_Percent[2], "%)"))

ggsave("outputs/figures/individuals_factor_map.png", p_individuals, width = 12, height = 8)
cat("Individuals factor map saved to: outputs/figures/individuals_factor_map.png\n")

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n3.7 PCA INTERPRETATION SUMMARY")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Interpret dimensions based on top contributing variables
cat("\nDIMENSION INTERPRETATION:\n")

# Interpret Dim1
cat("\nDIMENSION 1 (", variance_table$Variance_Percent[1], "% of variance):\n", sep = "")
dim1_vars <- names(head(dim1_contrib, 5))
dim1_contrib_values <- round(head(dim1_contrib, 5), 2)
for(i in 1:length(dim1_vars)) {
  cat("   -", dim1_vars[i], ":", dim1_contrib_values[i], "%\n")
}

# Interpret Dim2
cat("\nDIMENSION 2 (", variance_table$Variance_Percent[2], "% of variance):\n", sep = "")
dim2_vars <- names(head(dim2_contrib, 5))
dim2_contrib_values <- round(head(dim2_contrib, 5), 2)
for(i in 1:length(dim2_vars)) {
  cat("   -", dim2_vars[i], ":", dim2_contrib_values[i], "%\n")
}

# Interpret Dim3
cat("\nDIMENSION 3 (", variance_table$Variance_Percent[3], "% of variance):\n", sep = "")
dim3_contrib <- sort(var_contrib[, 3], decreasing = TRUE)
dim3_vars <- names(head(dim3_contrib, 5))
dim3_contrib_values <- round(head(dim3_contrib, 5), 2)
for(i in 1:length(dim3_vars)) {
  cat("   -", dim3_vars[i], ":", dim3_contrib_values[i], "%\n")
}

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n3.8 SAVE PCA RESULTS FOR STEP 4")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Extract PCA coordinates for clustering
pca_coordinates <- as.data.frame(pca_result$ind$coord)
names(pca_coordinates) <- paste0("Dim", 1:ncol(pca_coordinates))

# Save PCA coordinates
write.csv(pca_coordinates, "outputs/tables/pca_coordinates.csv", row.names = FALSE)
cat("PCA coordinates saved to: outputs/tables/pca_coordinates.csv\n")

# Save summary report
sink("outputs/tables/pca_summary_report.txt")
cat("PCA SUMMARY REPORT\n")
cat("==================\n\n")
cat("Date:", Sys.Date(), "\n\n")
cat("Dataset:", nrow(numeric_vars), "observations,", ncol(numeric_vars), "variables\n\n")
cat("EXPLAINED VARIANCE:\n")
print(variance_table)
cat("\n\nDIMENSION 1 - TOP CONTRIBUTORS:\n")
print(top_dim1)
cat("\n\nDIMENSION 2 - TOP CONTRIBUTORS:\n")
print(top_dim2)
cat("\n\nQUALITY OF REPRESENTATION (COS2):\n")
print(head(cos2_table, 15))
sink()

cat("PCA summary report saved to: outputs/tables/pca_summary_report.txt\n")

# Final message
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\nSTEP 3 COMPLETE!")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

cat("\nPCA SUMMARY:\n")
cat("  - Total variance explained by first 2 dimensions:", 
    round(variance_table$Cumulative_Percent[2], 2), "%\n")
cat("  - Total variance explained by first 3 dimensions:", 
    round(variance_table$Cumulative_Percent[3], 2), "%\n")
cat("  - Recommended dimensions for interpretation:", n_interpret, "\n")
cat("  - Number of variables with good representation (cos2 > 0.5):", 
    nrow(well_represented), "\n")

cat("\nOUTPUTS GENERATED:\n")
cat("  - outputs/figures/scree_plot.png\n")
cat("  - outputs/figures/variable_factor_map_dim1_dim2.png\n")
cat("  - outputs/figures/variable_factor_map_dim2_dim3.png\n")
cat("  - outputs/figures/individuals_factor_map.png\n")
cat("  - outputs/tables/variable_contributions.txt\n")
cat("  - outputs/tables/pca_coordinates.csv\n")
cat("  - outputs/tables/pca_summary_report.txt\n")
cat("  - data/pca_result.rds\n")

cat("\nNext step: Run 04_clustering_analysis.R\n")

