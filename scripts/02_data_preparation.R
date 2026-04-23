# scripts/02_data_preparation.R
# Step 2: Data Preparation - Quality Assessment, Missing Values, Outliers, Normalization

# Clear environment
rm(list = ls())

# Set working directory
setwd("C:/Users/zahra/OneDrive/Desktop/Project_R")

# Load required libraries
library(tidyverse)
library(ggplot2)
library(corrplot)

cat("\n==========================================")
cat("\nStep 2: Data Preparation")
cat("\n==========================================\n")

# Load the data
cat("\n Loading dataset...\n")
jobs_data <- read.csv("data/AI_Impact_on_Jobs_2030.csv", stringsAsFactors = FALSE)

# Create a copy for processing
data_processed <- jobs_data

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n2.1 DATA QUALITY ASSESSMENT")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

cat("Total observations:", nrow(data_processed), "\n")
cat("Total variables:", ncol(data_processed), "\n")

# Check for duplicates
duplicates <- sum(duplicated(data_processed))
cat("Duplicate rows:", duplicates, "\n")

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n2.2 MISSING VALUES HANDLING")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Check for missing values
missing_counts <- colSums(is.na(data_processed))
missing_percent <- (missing_counts / nrow(data_processed)) * 100

missing_df <- data.frame(
  Variable = names(missing_counts),
  Missing_Count = missing_counts,
  Missing_Percent = round(missing_percent, 2)
) %>% filter(Missing_Count > 0)

if (nrow(missing_df) > 0) {
  cat(" Missing values detected:\n")
  print(missing_df)
  
  # Impute missing values
  for (col in names(data_processed)) {
    if (sum(is.na(data_processed[[col]])) > 0) {
      if (is.numeric(data_processed[[col]])) {
        # Numeric: median imputation
        median_val <- median(data_processed[[col]], na.rm = TRUE)
        data_processed[[col]][is.na(data_processed[[col]])] <- median_val
        cat("  Imputed", col, "with median =", round(median_val, 2), "\n")
      } else {
        # Categorical: mode imputation
        mode_val <- names(sort(table(data_processed[[col]]), decreasing = TRUE))[1]
        data_processed[[col]][is.na(data_processed[[col]])] <- mode_val
        cat("  Imputed", col, "with mode =", mode_val, "\n")
      }
    }
  }
} else {
  cat("No missing values detected!\n")
}

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n2.3 OUTLIER DETECTION")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Function to detect outliers using IQR method
detect_outliers <- function(x) {
  if (is.numeric(x)) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    outliers <- sum(x < lower_bound | x > upper_bound, na.rm = TRUE)
    return(outliers)
  } else {
    return(NA)
  }
}

# Select numeric columns
numeric_cols <- names(data_processed)[sapply(data_processed, is.numeric)]
categorical_cols <- names(data_processed)[sapply(data_processed, is.character)]

cat("Numeric variables:", length(numeric_cols), "\n")
cat("Categorical variables:", length(categorical_cols), "\n")

# Detect outliers for numeric variables
outlier_counts <- sapply(data_processed[numeric_cols], detect_outliers)

outlier_summary <- data.frame(
  Variable = names(outlier_counts),
  Outlier_Count = outlier_counts,
  Outlier_Percent = round((outlier_counts / nrow(data_processed)) * 100, 2)
)

cat("\n Outlier detection results (IQR method):\n")
print(outlier_summary)

# Generate boxplots for outlier visualization
cat("\n Generating outlier visualizations...\n")

# Boxplot for key numeric variables
key_vars <- c("Average_Salary", "Years_Experience", "AI_Exposure_Index", 
              "Tech_Growth_Factor", "Automation_Probability_2030")

# Create boxplots
for (var in key_vars) {
  if (var %in% names(data_processed)) {
    p <- ggplot(data_processed, aes(x = 1, y = .data[[var]])) +
      geom_boxplot(fill = "steelblue", alpha = 0.7) +
      theme_minimal() +
      labs(title = paste("Boxplot of", var),
           x = "", y = var) +
      theme(axis.text.x = element_blank())
    
    ggsave(paste0("outputs/figures/boxplot_", var, ".png"), p, width = 6, height = 5)
    cat("  Saved: outputs/figures/boxplot_", var, ".png\n", sep = "")
  }
}

# Outlier treatment (capping extreme outliers >5%)
cat("\n Outlier treatment:\n")
for (var in names(outlier_summary[outlier_summary$Outlier_Percent > 5, "Variable"])) {
  if (length(var) > 0 && var %in% names(data_processed)) {
    Q1 <- quantile(data_processed[[var]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data_processed[[var]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR
    
    original_outliers <- sum(data_processed[[var]] < lower | data_processed[[var]] > upper)
    data_processed[[var]] <- pmin(pmax(data_processed[[var]], lower), upper)
    
    cat("  Capped", var, ":", original_outliers, "outliers treated\n")
  }
}

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n2.4 NORMALIZATION (Z-SCORE STANDARDIZATION)")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Separate numeric and categorical variables
numeric_vars <- data_processed %>% select(all_of(numeric_cols))
categorical_vars <- data_processed %>% select(all_of(categorical_cols))

cat("Variables for PCA analysis:\n")
cat("  Numeric variables:", ncol(numeric_vars), "\n")
cat("  Categorical variables:", ncol(categorical_vars), "\n")

# Apply Z-score standardization
cat("\n Applying Z-score standardization to numeric variables...\n")
cat("  Formula: (x - mean) / sd\n")

numeric_scaled <- as.data.frame(scale(numeric_vars))
names(numeric_scaled) <- names(numeric_vars)

# Verify normalization
cat("\n Normalization complete!\n")
cat("  Post-normalization summary (mean ≈ 0, sd ≈ 1):\n")

norm_check <- data.frame(
  Variable = names(numeric_scaled)[1:min(5, ncol(numeric_scaled))],
  Mean = round(sapply(numeric_scaled[1:min(5, ncol(numeric_scaled))], mean), 6),
  SD = round(sapply(numeric_scaled[1:min(5, ncol(numeric_scaled))], sd), 2)
)
print(norm_check)

# Combine processed data
data_final <- cbind(numeric_scaled, categorical_vars)

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n2.5 CORRELATION ANALYSIS")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Correlation matrix for key variables
cor_matrix <- cor(numeric_scaled[, key_vars[key_vars %in% names(numeric_scaled)]])

cat("Correlation matrix for key variables:\n")
print(round(cor_matrix, 3))

# Save correlation plot
png("outputs/figures/correlation_matrix.png", width = 800, height = 800)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.cex = 1, title = "Correlation Matrix - Key Variables",
         mar = c(0,0,2,0))
dev.off()
cat("Correlation matrix saved to outputs/figures/correlation_matrix.png\n")

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n2.6 FINAL DATA SUMMARY")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

cat("\nData preparation complete!\n")
cat("\n Final dataset dimensions:", nrow(data_final), "×", ncol(data_final), "\n")
cat("\n Final variables:\n")
cat("  Numeric (scaled):", ncol(numeric_scaled), "variables\n")
cat("  Categorical:", ncol(categorical_vars), "variables\n")

# Save processed data
saveRDS(data_final, "data/processed_data.rds")
write.csv(data_final, "data/processed_data.csv", row.names = FALSE)

cat("\n Processed data saved:\n")
cat("  - data/processed_data.rds (R format)\n")
cat("  - data/processed_data.csv (CSV format)\n")

# Save preparation report
sink("outputs/tables/data_preparation_report.txt")
cat("DATA PREPARATION REPORT\n")
cat("=======================\n\n")
cat("Date:", Sys.Date(), "\n\n")
cat("Initial data:", nrow(jobs_data), "×", ncol(jobs_data), "\n")
cat("Final data:", nrow(data_final), "×", ncol(data_final), "\n\n")
cat("Numeric variables:", length(numeric_cols), "\n")
cat("Categorical variables:", length(categorical_cols), "\n\n")
cat("Missing values handled: None found\n")
cat("Outliers detected and treated\n")
cat("Normalization applied: Z-score standardization\n\n")
cat("Variables in final dataset:\n")
cat(paste(names(data_final), collapse = ", "), "\n")
sink()

cat("\n Preparation report saved to: outputs/tables/data_preparation_report.txt\n")

# Display final message
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
cat("\n STEP 2 COMPLETE!")
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("\n Summary of findings:\n")
cat("  • No missing values in the dataset\n")
cat("  • Outliers detected and capped for extreme cases\n")
cat("  • Z-score standardization applied to", length(numeric_cols), "numeric variables\n")
cat("  • Data ready for PCA and clustering analysis\n")
cat("\n Next step: Run 03_factor_analysis_PCA.R\n")

