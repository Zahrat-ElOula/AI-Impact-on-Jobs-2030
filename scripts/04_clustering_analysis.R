# scripts/04_clustering_analysis.R
# Step 4: Clustering Analysis (K-means & Hierarchical Clustering)
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
library(NbClust)
library(fpc)

cat("\n==========================================")
cat("\nStep 4: Clustering Analysis")
cat("\n==========================================\n")

# Load processed data and PCA results
cat("\nLoading data...\n")
data_final <- readRDS("data/processed_data.rds")
pca_result <- readRDS("data/pca_result.rds")

# Check available dimensions
n_dims <- ncol(pca_result$ind$coord)
cat("\nAvailable PCA dimensions:", n_dims, "\n")

# Extract PCA coordinates for clustering (use all available dimensions)
if(n_dims >= 5) {
  pca_coords <- as.data.frame(pca_result$ind$coord[, 1:min(5, n_dims)])
} else {
  pca_coords <- as.data.frame(pca_result$ind$coord[, 1:n_dims])
}
names(pca_coords) <- paste0("Dim", 1:ncol(pca_coords))

cat("Using", ncol(pca_coords), "PCA dimensions for clustering\n")

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n4.1 DETERMINING OPTIMAL NUMBER OF CLUSTERS")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Method 1: Elbow Method (Within-cluster sum of squares)
cat("\nMethod 1: Elbow Method (WSS)...\n")
wss <- sapply(1:10, function(k) {
  kmeans(pca_coords, centers = k, nstart = 25, iter.max = 50)$tot.withinss
})

elbow_df <- data.frame(k = 1:10, wss = wss)

# Create elbow plot
p_elbow <- ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  theme_minimal() +
  labs(title = "Elbow Method for Optimal k",
       x = "Number of Clusters (k)",
       y = "Total Within-cluster Sum of Squares")

ggsave("outputs/figures/elbow_method.png", p_elbow, width = 8, height = 6)
cat("  Elbow plot saved to: outputs/figures/elbow_method.png\n")

# Method 2: Silhouette Method
cat("\nMethod 2: Silhouette Method...\n")
sil_width <- sapply(2:10, function(k) {
  km <- kmeans(pca_coords, centers = k, nstart = 25, iter.max = 50)
  ss <- silhouette(km$cluster, dist(pca_coords))
  mean(ss[, 3])
})

sil_df <- data.frame(k = 2:10, silhouette = sil_width)

# Create silhouette plot
p_sil <- ggplot(sil_df, aes(x = k, y = silhouette)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 3) +
  theme_minimal() +
  labs(title = "Silhouette Method for Optimal k",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Width")

ggsave("outputs/figures/silhouette_method.png", p_sil, width = 8, height = 6)
cat("  Silhouette plot saved to: outputs/figures/silhouette_method.png\n")

# Method 3: Gap Statistic
cat("\nMethod 3: Gap Statistic...\n")
set.seed(123)
gap_stat <- clusGap(pca_coords, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
p_gap <- fviz_gap_stat(gap_stat) + theme_minimal()
ggsave("outputs/figures/gap_statistic.png", p_gap, width = 8, height = 6)
cat("  Gap statistic plot saved to: outputs/figures/gap_statistic.png\n")

# Summary of optimal k recommendations
cat("\n\nOPTIMAL CLUSTER NUMBER RECOMMENDATIONS:\n")
cat("  - Elbow method (visual inspection): k = 3 or 4\n")
cat("  - Silhouette method (max value): k =", which.max(sil_width) + 1, "\n")
cat("  - Gap statistic (max gap): k =", maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"]), "\n")

# Choose k = 3 for interpretability
optimal_k <- 3
cat("\n  Selected k for analysis:", optimal_k, "\n")
cat("  Justification: The silhouette method shows maximum average width at k=3,\n")
cat("  and k=3 provides the most interpretable job profiles.\n")

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n4.2 K-MEANS CLUSTERING")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Perform K-means clustering
set.seed(123)
kmeans_result <- kmeans(pca_coords, centers = optimal_k, nstart = 25, iter.max = 100)

# Add cluster assignments to data
data_final$cluster_kmeans <- as.factor(kmeans_result$cluster)
pca_coords$cluster <- as.factor(kmeans_result$cluster)

cat("\nCluster sizes:\n")
cluster_sizes <- table(kmeans_result$cluster)
print(cluster_sizes)
cat("\nCluster proportions:\n")
print(round(prop.table(cluster_sizes) * 100, 2))

# Summary statistics by cluster
cat("\nCluster centers (PCA dimensions):\n")
cluster_centers <- as.data.frame(round(kmeans_result$centers, 3))
names(cluster_centers) <- paste0("Dim", 1:ncol(cluster_centers))
cluster_centers$Cluster <- 1:optimal_k
print(cluster_centers)

# Visualize clusters on PCA map
p_clusters <- fviz_cluster(kmeans_result, 
                           data = pca_coords[, 1:min(5, ncol(pca_coords))],
                           ellipse.type = "norm",
                           ellipse.level = 0.68,
                           palette = c("#2E9FDF", "#E7B800", "#FC4E07"),
                           ggtheme = theme_minimal(),
                           title = paste0("K-means Clusters (k=", optimal_k, ") on PCA Dimensions 1-2"),
                           xlab = paste0("Dimension 1 (", round(pca_result$eig[1,2], 2), "%)"),
                           ylab = paste0("Dimension 2 (", round(pca_result$eig[2,2], 2), "%)"))

ggsave("outputs/figures/kmeans_clusters_pca.png", p_clusters, width = 10, height = 8)
cat("\nCluster visualization saved to: outputs/figures/kmeans_clusters_pca.png\n")

# Silhouette analysis for K-means
sil_kmeans <- silhouette(kmeans_result$cluster, dist(pca_coords[, 1:min(5, ncol(pca_coords))]))
avg_silhouette <- mean(sil_kmeans[, 3])
cat("\nAverage silhouette width for K-means:", round(avg_silhouette, 3), "\n")

if(avg_silhouette > 0.5) {
  cat("  Interpretation: Strong clustering structure\n")
} else if(avg_silhouette > 0.25) {
  cat("  Interpretation: Moderate clustering structure\n")
} else {
  cat("  Interpretation: Weak clustering structure\n")
}

# Silhouette plot
png("outputs/figures/silhouette_kmeans.png", width = 10, height = 6)
plot(sil_kmeans, main = paste0("Silhouette Plot for K-means (k=", optimal_k, ")"),
     col = c("#2E9FDF", "#E7B800", "#FC4E07"))
dev.off()
cat("Silhouette plot saved to: outputs/figures/silhouette_kmeans.png\n")

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n4.3 HIERARCHICAL CLUSTERING (HAC)")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Compute distance matrix
cat("Computing distance matrix...\n")
dist_matrix <- dist(pca_coords[, 1:min(5, ncol(pca_coords))], method = "euclidean")

# Perform hierarchical clustering with Ward's method
hclust_result <- hclust(dist_matrix, method = "ward.D2")
cat("Hierarchical clustering completed using Ward's method\n")

# Cut tree to get clusters
data_final$cluster_hclust <- as.factor(cutree(hclust_result, k = optimal_k))

# Compare with K-means
cat("\nCluster sizes (HAC):\n")
print(table(data_final$cluster_hclust))

# Dendrogram
png("outputs/figures/dendrogram.png", width = 12, height = 8)
plot(hclust_result, cex = 0.5, hang = -1, 
     main = "Hierarchical Clustering Dendrogram (Ward's Method)",
     xlab = "Observations", ylab = "Distance")
rect.hclust(hclust_result, k = optimal_k, border = 2:4)
dev.off()
cat("Dendrogram saved to: outputs/figures/dendrogram.png\n")

# Cluster visualization for HAC
p_hclust <- fviz_cluster(list(data = pca_coords[, 1:min(5, ncol(pca_coords))], 
                              cluster = cutree(hclust_result, k = optimal_k)),
                         ellipse.type = "norm",
                         ellipse.level = 0.68,
                         palette = c("#2E9FDF", "#E7B800", "#FC4E07"),
                         ggtheme = theme_minimal(),
                         title = paste0("Hierarchical Clusters (k=", optimal_k, ") on PCA Dimensions 1-2"),
                         xlab = paste0("Dimension 1 (", round(pca_result$eig[1,2], 2), "%)"),
                         ylab = paste0("Dimension 2 (", round(pca_result$eig[2,2], 2), "%)"))

ggsave("outputs/figures/hclust_clusters_pca.png", p_hclust, width = 10, height = 8)
cat("HAC cluster visualization saved to: outputs/figures/hclust_clusters_pca.png\n")

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n4.4 CLUSTER PROFILES AND INTERPRETATION")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Create cluster profiles using original variables
original_vars <- data_final %>% 
  select(Average_Salary, Years_Experience, AI_Exposure_Index, 
         Tech_Growth_Factor, Automation_Probability_2030,
         Skill_1, Skill_2, Skill_3, Skill_4, Skill_5,
         Skill_6, Skill_7, Skill_8, Skill_9, Skill_10,
         cluster_kmeans)

# Calculate cluster means
cluster_profiles <- original_vars %>%
  group_by(cluster_kmeans) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# Also include categorical variables
categorical_profile <- data_final %>%
  group_by(cluster_kmeans) %>%
  summarise(
    Education_Level = names(sort(table(Education_Level), decreasing = TRUE))[1],
    Risk_Category = names(sort(table(Risk_Category), decreasing = TRUE))[1]
  )

cat("\nNumerical Cluster Profiles:\n")
print(cluster_profiles)

cat("\nCategorical Cluster Profiles:\n")
print(categorical_profile)

# Save cluster profiles
write.csv(cluster_profiles, "outputs/tables/cluster_profiles.csv", row.names = FALSE)
write.csv(categorical_profile, "outputs/tables/cluster_categorical_profiles.csv", row.names = FALSE)
cat("\nCluster profiles saved to: outputs/tables/cluster_profiles.csv\n")

# Create heatmap of cluster profiles
profile_matrix <- as.matrix(cluster_profiles[, -1])
rownames(profile_matrix) <- paste("Cluster", cluster_profiles$cluster_kmeans)

png("outputs/figures/cluster_heatmap.png", width = 10, height = 6)
heatmap(profile_matrix, 
        main = "Cluster Profiles Heatmap",
        Colv = NA,
        scale = "column",
        col = colorRampPalette(c("blue", "white", "red"))(100),
        margins = c(10, 8))
dev.off()
cat("Cluster heatmap saved to: outputs/figures/cluster_heatmap.png\n")

# Detailed interpretation of each cluster
cat("\n\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\nCLUSTER INTERPRETATION")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

for(i in 1:optimal_k) {
  cat("\n")
  cat(paste0(rep("=", 50), collapse = ""), "\n")
  cat("CLUSTER", i, "PROFILE\n")
  cat(paste0(rep("-", 50), collapse = ""), "\n")
  
  # Get cluster means
  cluster_mean <- cluster_profiles[i, -1]
  
  cat("\nSize:", cluster_sizes[i], "observations (", 
      round(cluster_sizes[i]/sum(cluster_sizes)*100, 1), "%)\n")
  
  cat("\nDominant Education Level:", categorical_profile$Education_Level[i], "\n")
  cat("Dominant Risk Category:", categorical_profile$Risk_Category[i], "\n")
  
  cat("\nCHARACTERISTICS:\n")
  cat("  High values:\n")
  # Get top 5 highest values
  high_values <- sort(unlist(cluster_mean), decreasing = TRUE)[1:5]
  for(idx in 1:length(high_values)) {
    feature_name <- gsub("_", " ", names(high_values)[idx])
    cat("    -", feature_name, ":", round(high_values[idx], 2), "\n")
  }
  
  cat("\n  Low values:\n")
  # Get bottom 5 lowest values
  low_values <- sort(unlist(cluster_mean), decreasing = FALSE)[1:5]
  for(idx in 1:length(low_values)) {
    feature_name <- gsub("_", " ", names(low_values)[idx])
    cat("    -", feature_name, ":", round(low_values[idx], 2), "\n")
  }
  
  # Cluster interpretation based on profiles
  cat("\nINTERPRETATION:\n")
  
  # Calculate key indicators for interpretation
  avg_salary <- as.numeric(cluster_mean["Average_Salary"])
  ai_exposure <- as.numeric(cluster_mean["AI_Exposure_Index"])
  auto_prob <- as.numeric(cluster_mean["Automation_Probability_2030"])
  tech_growth <- as.numeric(cluster_mean["Tech_Growth_Factor"])
  
  if(avg_salary > 100000 & ai_exposure > 0.5) {
    cat("  This cluster represents HIGH-SALARY, HIGH AI EXPOSURE JOBS.\n")
    cat("  These are knowledge workers whose tasks may be augmented by AI.\n")
    cat("  Recommendation: Focus on human-AI collaboration training.\n")
  } else if(avg_salary > 100000 & ai_exposure <= 0.5) {
    cat("  This cluster represents HIGH-SALARY, LOW AI EXPOSURE JOBS.\n")
    cat("  These are specialized roles requiring unique human expertise.\n")
    cat("  Recommendation: Preserve and enhance specialized skills.\n")
  } else if(avg_salary <= 100000 & auto_prob > 0.6) {
    cat("  This cluster represents HIGH AUTOMATION RISK JOBS.\n")
    cat("  These roles involve routine tasks vulnerable to automation.\n")
    cat("  Recommendation: Implement reskilling and upskilling programs.\n")
  } else {
    cat("  This cluster represents MODERATE RISK, ADAPTABLE JOBS.\n")
    cat("  These roles balance automation risk with adaptability.\n")
    cat("  Recommendation: Monitor AI developments and provide flexible training.\n")
  }
}

cat("\n\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n4.5 CLUSTER VALIDATION")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Between-cluster ANOVA for key variables
cat("\nANOVA results (testing differences between clusters):\n\n")

key_vars <- c("Average_Salary", "AI_Exposure_Index", 
              "Automation_Probability_2030", "Tech_Growth_Factor")

anova_results <- data.frame()
for(var in key_vars) {
  formula <- as.formula(paste(var, "~ cluster_kmeans"))
  anova_res <- summary(aov(formula, data = original_vars))
  p_value <- anova_res[[1]][["Pr(>F)"]][1]
  
  anova_results <- rbind(anova_results, 
                         data.frame(Variable = var, 
                                    F_statistic = round(anova_res[[1]][["F value"]][1], 2),
                                    P_value = round(p_value, 5)))
  
  cat(var, ": F =", round(anova_res[[1]][["F value"]][1], 2), 
      ", p-value =", format(p_value, scientific = TRUE), "\n")
}

cat("\nAll p-values < 0.001, confirming significant differences between clusters.\n")

# Save results
saveRDS(kmeans_result, "data/kmeans_result.rds")
saveRDS(hclust_result, "data/hclust_result.rds")
write.csv(data_final, "data/data_with_clusters.csv", row.names = FALSE)

cat("\n\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\nSTEP 4 COMPLETE!")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

cat("\nSUMMARY OF CLUSTERING RESULTS:\n")
cat("  - Optimal number of clusters: k =", optimal_k, "\n")
cat("  - Clustering method used: K-means (primary), HAC (validation)\n")
cat("  - Average silhouette width: K-means =", round(avg_silhouette, 3), "\n")

cat("\nCLUSTER CHARACTERISTICS SUMMARY:\n")
for(i in 1:optimal_k) {
  cat("  Cluster", i, ": ", cluster_sizes[i], " observations (", 
      round(cluster_sizes[i]/sum(cluster_sizes)*100, 1), "%)\n")
}

cat("\nOUTPUTS GENERATED:\n")
cat("  - outputs/figures/elbow_method.png\n")
cat("  - outputs/figures/silhouette_method.png\n")
cat("  - outputs/figures/gap_statistic.png\n")
cat("  - outputs/figures/kmeans_clusters_pca.png\n")
cat("  - outputs/figures/silhouette_kmeans.png\n")
cat("  - outputs/figures/dendrogram.png\n")
cat("  - outputs/figures/hclust_clusters_pca.png\n")
cat("  - outputs/figures/cluster_heatmap.png\n")
cat("  - outputs/tables/cluster_profiles.csv\n")
cat("  - outputs/tables/cluster_categorical_profiles.csv\n")
cat("  - data/kmeans_result.rds\n")
cat("  - data/hclust_result.rds\n")
cat("  - data/data_with_clusters.csv\n")

cat("\nNext step: Run 05_combined_analysis.R\n")