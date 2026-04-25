# scripts/05_combined_analysis.R
# Step 5: Combined Analysis - Relationship between PCA and Clustering
# AI Impact on Jobs 2030 Dataset

# Clear environment
rm(list = ls())

# Set working directory
setwd("C:/Users/zahra/OneDrive/Desktop/Project_R")

# Load required libraries
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
library(gridExtra)
library(corrplot)

cat("\n==========================================")
cat("\nStep 5: Combined Analysis (PCA + Clustering)")
cat("\n==========================================\n")

# Load data and results
cat("\nLoading data and results...\n")
data_final <- read.csv("data/data_with_clusters.csv")
pca_result <- readRDS("data/pca_result.rds")
kmeans_result <- readRDS("data/kmeans_result.rds")

# Load PCA coordinates
pca_coords <- as.data.frame(pca_result$ind$coord[, 1:5])
names(pca_coords) <- paste0("Dim", 1:5)
pca_coords$cluster <- as.factor(data_final$cluster_kmeans)

cat("Data loaded successfully!")
cat("\n  Observations:", nrow(data_final))
cat("\n  Clusters:", length(unique(data_final$cluster_kmeans)))

cat("\n\n==================================================================")
cat("\n5.1 CLUSTER VISUALIZATION ON PCA MAP")
cat("\n==================================================================\n")

# Plot 1: Clusters on PCA dimensions 1-2 with confidence ellipses
p_clusters_12 <- fviz_cluster(kmeans_result, 
                              data = pca_coords[, 1:5],
                              ellipse.type = "norm",
                              ellipse.level = 0.68,
                              palette = c("#2E9FDF", "#E7B800", "#FC4E07"),
                              ggtheme = theme_minimal(),
                              main = "K-means Clusters on PCA Dimensions 1-2",
                              xlab = paste0("Dimension 1 (", round(pca_result$eig[1,2], 2), "%)"),
                              ylab = paste0("Dimension 2 (", round(pca_result$eig[2,2], 2), "%)"))

ggsave("outputs/figures/combined_clusters_dim1_dim2.png", p_clusters_12, width = 10, height = 8)
cat("Cluster visualization (Dim1-Dim2) saved to: outputs/figures/combined_clusters_dim1_dim2.png\n")

# Plot 2: Clusters on PCA dimensions 2-3
p_clusters_23 <- fviz_cluster(kmeans_result, 
                              data = pca_coords[, 1:5],
                              axes = c(2, 3),
                              ellipse.type = "norm",
                              ellipse.level = 0.68,
                              palette = c("#2E9FDF", "#E7B800", "#FC4E07"),
                              ggtheme = theme_minimal(),
                              main = "K-means Clusters on PCA Dimensions 2-3",
                              xlab = paste0("Dimension 2 (", round(pca_result$eig[2,2], 2), "%)"),
                              ylab = paste0("Dimension 3 (", round(pca_result$eig[3,2], 2), "%)"))

ggsave("outputs/figures/combined_clusters_dim2_dim3.png", p_clusters_23, width = 10, height = 8)
cat("Cluster visualization (Dim2-Dim3) saved to: outputs/figures/combined_clusters_dim2_dim3.png\n")

cat("\n==================================================================")
cat("\n5.2 CLUSTER MEANS ON PCA DIMENSIONS")
cat("\n==================================================================\n")

# Calculate and display cluster means for PCA dimensions - FIXED
cluster_pca_means <- pca_coords %>%
  group_by(cluster) %>%
  summarise(
    Dim1 = mean(Dim1),
    Dim2 = mean(Dim2),
    Dim3 = mean(Dim3),
    Dim4 = mean(Dim4),
    Dim5 = mean(Dim5),
    .groups = "drop"
  )

cat("\nCluster mean coordinates on PCA dimensions:\n")
# Convert to data frame and print without rounding the cluster column
cluster_means_print <- cluster_pca_means
cluster_means_print$cluster <- as.character(cluster_means_print$cluster)
cluster_means_print[, 2:6] <- round(cluster_means_print[, 2:6], 3)
print(cluster_means_print)

# Create bar plot of cluster means
cluster_means_long <- cluster_pca_means %>%
  pivot_longer(cols = Dim1:Dim5, names_to = "Dimension", values_to = "Mean")

p_means_bars <- ggplot(cluster_means_long, aes(x = Dimension, y = Mean, fill = cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(title = "Cluster Mean Positions on PCA Dimensions",
       x = "PCA Dimension", y = "Mean Coordinate",
       fill = "Cluster") +
  scale_fill_manual(values = c("#2E9FDF", "#E7B800", "#FC4E07"))

ggsave("outputs/figures/cluster_pca_means.png", p_means_bars, width = 10, height = 6)
cat("Cluster means bar plot saved to: outputs/figures/cluster_pca_means.png\n")

cat("\n==================================================================")
cat("\n5.3 VARIABLE CONTRIBUTIONS TO CLUSTER SEPARATION")
cat("\n==================================================================\n")

# One-way ANOVA to identify which variables most differentiate clusters
cat("\nANOVA F-statistics (higher = better separation):\n")

# Select numeric variables
numeric_vars <- data_final %>%
  select(Average_Salary, Years_Experience, AI_Exposure_Index,
         Tech_Growth_Factor, Automation_Probability_2030,
         Skill_1, Skill_2, Skill_3, Skill_4, Skill_5,
         Skill_6, Skill_7, Skill_8, Skill_9, Skill_10)

# Calculate ANOVA F-statistics for each variable
f_stats <- sapply(names(numeric_vars), function(var) {
  formula <- as.formula(paste(var, "~ cluster_kmeans"))
  anova_res <- summary(aov(formula, data = data_final))
  return(anova_res[[1]][["F value"]][1])
})

f_stats_df <- data.frame(
  Variable = names(f_stats),
  F_statistic = round(f_stats, 2)
) %>% arrange(desc(F_statistic))

cat("\nTop 10 variables by F-statistic:\n")
print(head(f_stats_df, 10))

# Save F-statistics
write.csv(f_stats_df, "outputs/tables/anova_f_statistics.csv", row.names = FALSE)
cat("\nANOVA F-statistics saved to: outputs/tables/anova_f_statistics.csv\n")

# Create bar plot of F-statistics
p_fstats <- ggplot(head(f_stats_df, 10), aes(x = reorder(Variable, F_statistic), y = F_statistic)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Variables Differentiating Clusters",
       x = "Variable", y = "ANOVA F-statistic")

ggsave("outputs/figures/top_separating_variables.png", p_fstats, width = 10, height = 6)
cat("F-statistics plot saved to: outputs/figures/top_separating_variables.png\n")

cat("\n==================================================================")
cat("\n5.4 CLUSTER INTERPRETATION USING PCA DIMENSIONS")
cat("\n==================================================================\n")

# Interpretation of each cluster based on PCA dimensions
cat("\nCluster interpretation based on PCA dimensions:\n")

for(i in 1:3) {
  cat("\n")
  cat(paste0(rep("-", 40), collapse = ""), "\n")
  cat("CLUSTER", i, "\n")
  cat(paste0(rep("-", 40), collapse = ""), "\n")
  
  # Get cluster center for this cluster
  cluster_center <- cluster_pca_means[i, c("Dim1", "Dim2", "Dim3", "Dim4", "Dim5")]
  cluster_center_numeric <- as.numeric(cluster_center)
  names(cluster_center_numeric) <- c("Dim1", "Dim2", "Dim3", "Dim4", "Dim5")
  
  # Find which dimensions have highest positive and negative values
  high_dims <- names(sort(cluster_center_numeric, decreasing = TRUE))[1:2]
  low_dims <- names(sort(cluster_center_numeric, decreasing = FALSE))[1:2]
  
  cat("  Characterized by:\n")
  cat("    High on:", paste(high_dims, collapse = ", "), "\n")
  cat("    Low on:", paste(low_dims, collapse = ", "), "\n")
  
  # Link to dimension interpretations from PCA
  cat("\n  Interpretation:\n")
  if(i == 1) {
    cat("    - High Dim2 (Cognitive & Analytical Skills): These jobs require\n")
    cat("      strong cognitive abilities and analytical thinking.\n")
    cat("    - High Dim1 (Economic & Technology Factor): Moderate economic value.\n")
    cat("    - These are knowledge worker positions that benefit from AI augmentation.\n")
  } else if(i == 2) {
    cat("    - Low Dim2 (Cognitive & Analytical Skills): Limited cognitive demands.\n")
    cat("    - Low Dim3 (AI Exposure & Adaptability): Lower adaptability.\n")
    cat("    - These jobs face high automation risk and require reskilling.\n")
  } else if(i == 3) {
    cat("    - High Dim3 (AI Exposure & Adaptability): These jobs have high\n")
    cat("      AI exposure but possess complementary technical skills.\n")
    cat("    - High Dim1 (Economic & Technology Factor): Higher salaries.\n")
    cat("    - Technical specialists who can work alongside AI systems.\n")
  }
}

cat("\n==================================================================")
cat("\n5.5 CLUSTER CHARACTERISTICS BY ORIGINAL VARIABLES")
cat("\n==================================================================\n")

# Calculate cluster profiles with original scale interpretation
cluster_original_means <- data_final %>%
  group_by(cluster_kmeans) %>%
  summarise(
    Avg_Salary = round(mean(Average_Salary), 0),
    Years_Exp = round(mean(Years_Experience), 1),
    AI_Exposure = round(mean(AI_Exposure_Index), 2),
    Tech_Growth = round(mean(Tech_Growth_Factor), 2),
    Auto_Prob = round(mean(Automation_Probability_2030), 2),
    .groups = "drop"
  )

cat("\nCluster profiles in original units:\n")
print(cluster_original_means)

# Create comparison table
comparison_table <- data_final %>%
  group_by(cluster_kmeans) %>%
  summarise(
    `Average_Salary` = paste0(round(mean(Average_Salary), 0), " (", 
                              round(sd(Average_Salary), 0), ")"),
    `Years_Experience` = paste0(round(mean(Years_Experience), 1), " (", 
                                round(sd(Years_Experience), 1), ")"),
    `AI_Exposure` = paste0(round(mean(AI_Exposure_Index), 2), " (", 
                           round(sd(AI_Exposure_Index), 2), ")"),
    `Tech_Growth` = paste0(round(mean(Tech_Growth_Factor), 2), " (", 
                           round(sd(Tech_Growth_Factor), 2), ")"),
    `Automation_Prob` = paste0(round(mean(Automation_Probability_2030), 2), " (", 
                               round(sd(Automation_Probability_2030), 2), ")"),
    .groups = "drop"
  )

cat("\nCluster comparison (Mean (SD)):\n")
print(comparison_table)

# Save comparison table
write.csv(comparison_table, "outputs/tables/cluster_comparison_table.csv", row.names = FALSE)

cat("\n==================================================================")
cat("\n5.6 CLUSTER DISTRIBUTION ACROSS CATEGORICAL VARIABLES")
cat("\n==================================================================\n")

# Education level distribution by cluster
edu_distribution <- data_final %>%
  group_by(cluster_kmeans, Education_Level) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(cluster_kmeans) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

cat("\nEducation Level Distribution by Cluster:\n")
print(edu_distribution)

# Risk category distribution by cluster
risk_distribution <- data_final %>%
  group_by(cluster_kmeans, Risk_Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(cluster_kmeans) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

cat("\nRisk Category Distribution by Cluster:\n")
print(risk_distribution)

# Save distributions
write.csv(edu_distribution, "outputs/tables/education_by_cluster.csv", row.names = FALSE)
write.csv(risk_distribution, "outputs/tables/risk_by_cluster.csv", row.names = FALSE)

# Create stacked bar plot for education
p_edu <- ggplot(edu_distribution, aes(x = cluster_kmeans, y = Percentage, fill = Education_Level)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Education Level Distribution by Cluster",
       x = "Cluster", y = "Percentage (%)") +
  scale_fill_brewer(palette = "Set2")

ggsave("outputs/figures/education_by_cluster.png", p_edu, width = 8, height = 6)

# Create stacked bar plot for risk
p_risk <- ggplot(risk_distribution, aes(x = cluster_kmeans, y = Percentage, fill = Risk_Category)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Risk Category Distribution by Cluster",
       x = "Cluster", y = "Percentage (%)") +
  scale_fill_brewer(palette = "Set1")

ggsave("outputs/figures/risk_by_cluster.png", p_risk, width = 8, height = 6)
cat("Distribution plots saved to outputs/figures/\n")

cat("\n==================================================================")
cat("\n5.7 BETWEEN-CLUSTER DISTANCE ANALYSIS")
cat("\n==================================================================\n")

# Calculate Euclidean distances between cluster centers
cluster_centers <- as.matrix(kmeans_result$centers[, 1:5])
dist_matrix <- as.matrix(dist(cluster_centers))

cat("\nEuclidean distances between cluster centers (PCA space):\n")
rownames(dist_matrix) <- paste("Cluster", 1:3)
colnames(dist_matrix) <- paste("Cluster", 1:3)
print(round(dist_matrix, 3))

# Calculate separation quality
avg_between_dist <- mean(dist_matrix[lower.tri(dist_matrix)])
avg_within_dist <- mean(kmeans_result$withinss / kmeans_result$size)

cat("\nSeparation Quality:\n")
cat("  Average between-cluster distance:", round(avg_between_dist, 3), "\n")
cat("  Average within-cluster distance:", round(avg_within_dist, 3), "\n")
cat("  Ratio (between/within):", round(avg_between_dist / avg_within_dist, 3), "\n")

if(avg_between_dist / avg_within_dist > 2) {
  cat("  Interpretation: Good cluster separation\n")
} else if(avg_between_dist / avg_within_dist > 1.5) {
  cat("  Interpretation: Moderate cluster separation\n")
} else {
  cat("  Interpretation: Limited separation (consistent with low silhouette width)\n")
}

cat("\n==================================================================")
cat("\n5.8 SUMMARY OF RELATIONSHIP BETWEEN PCA AND CLUSTERING")
cat("\n==================================================================\n")

cat("\nKey Findings:\n")
cat("  1. PCA dimensions explain how clusters differ:\n")
cat("     - Dim2 (Cognitive Skills) separates Cluster 1 (+) from Cluster 2 (-)\n")
cat("     - Dim3 (AI Exposure) uniquely identifies Cluster 3 (+)\n")
cat("     - Dim1 (Economic) distinguishes Cluster 3 (+) from others\n")
cat("\n  2. Cluster separation is most pronounced on dimensions 2 and 3:\n")
cat("     - Between-cluster distance: 1.4 to 2.2 units in PCA space\n")
cat("     - Clusters are separated across multiple dimensions\n")
cat("\n  3. The multidimensional nature of the data means:\n")
cat("     - 2D visualization loses information\n")
cat("     - 5 PCA dimensions capture 36% of variance\n")
cat("     - Clusters represent archetypes rather than hard boundaries\n")

# Save combined analysis report
sink("outputs/tables/combined_analysis_report.txt")
cat("COMBINED ANALYSIS REPORT: PCA + CLUSTERING\n")
cat("===========================================\n\n")
cat("Date:", Sys.Date(), "\n\n")
cat("DATASET:\n")
cat("  Observations:", nrow(data_final), "\n")
cat("  Clusters:", length(unique(data_final$cluster_kmeans)), "\n\n")

cat("CLUSTER SIZES:\n")
for(i in 1:3) {
  cat("  Cluster", i, ":", sum(data_final$cluster_kmeans == i), "observations\n")
}
cat("\n")

cat("CLUSTER MEANS ON PCA DIMENSIONS:\n")
# Convert to data frame for printing
cluster_means_report <- cluster_pca_means
cluster_means_report$cluster <- as.character(cluster_means_report$cluster)
cluster_means_report[, 2:6] <- round(cluster_means_report[, 2:6], 3)
print(cluster_means_report)
cat("\n")

cat("TOP SEPARATING VARIABLES (ANOVA F-statistics):\n")
print(head(f_stats_df, 10))
cat("\n")

cat("CLUSTER CENTER DISTANCES:\n")
print(round(dist_matrix, 3))
cat("\n")

cat("CLUSTER PROFILES (Original Units):\n")
print(cluster_original_means)
cat("\n")

cat("CONCLUSIONS:\n")
cat("  - Three distinct job clusters identified\n")
cat("  - Clusters align with PCA dimensions 2 and 3\n")
cat("  - Skill variables are primary discriminators\n")
cat("  - Low silhouette width reflects data multidimensionality\n")
cat("  - Clusters provide actionable segmentation for workforce planning\n")
sink()

cat("\nCombined analysis report saved to: outputs/tables/combined_analysis_report.txt\n")

cat("\n==================================================================")
cat("\nSTEP 5 COMPLETE!")
cat("\n==================================================================\n")

cat("\nOUTPUTS GENERATED:\n")
cat("  - outputs/figures/combined_clusters_dim1_dim2.png\n")
cat("  - outputs/figures/combined_clusters_dim2_dim3.png\n")
cat("  - outputs/figures/cluster_pca_means.png\n")
cat("  - outputs/figures/top_separating_variables.png\n")
cat("  - outputs/figures/education_by_cluster.png\n")
cat("  - outputs/figures/risk_by_cluster.png\n")
cat("  - outputs/tables/anova_f_statistics.csv\n")
cat("  - outputs/tables/cluster_comparison_table.csv\n")
cat("  - outputs/tables/education_by_cluster.csv\n")
cat("  - outputs/tables/risk_by_cluster.csv\n")
cat("  - outputs/tables/combined_analysis_report.txt\n")

cat("\nNext step: Run 06_interpretation_report.R\n")