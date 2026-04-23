# scripts/01_load_data.R
# Step 1: Problem Understanding and Data Loading
# AI Impact on Jobs 2030 Dataset

# Clear environment
rm(list = ls())

# Set working directory
setwd("C:/Users/zahra/OneDrive/Desktop/Project_R")

# Load required libraries
library(tidyverse)
library(knitr)

cat("\n==========================================")
cat("\nStep 1: Problem Understanding & Data Loading")
cat("\n==========================================\n")

# Check if data file exists
data_file <- "data/AI_Impact_on_Jobs_2030.csv"

if (!file.exists(data_file)) {
  cat("\n Error: Data file not found at:", data_file, "\n")
  cat("Please place the CSV file in the 'data/' folder\n")
  stop("Data file missing!")
}

# Load the dataset
cat("\n Loading dataset...\n")
jobs_data <- read.csv(data_file, stringsAsFactors = FALSE)

# Display basic information
cat("\n Dataset loaded successfully!\n")
cat("\n Dataset Dimensions:", nrow(jobs_data), "rows ×", ncol(jobs_data), "columns\n")

# Display column names
cat("\n Variable names:\n")
print(names(jobs_data))

# Display structure
cat("\n Data structure:\n")
str(jobs_data)

# Display first few rows
cat("\n First 5 rows of data:\n")
print(head(jobs_data, 5))

# Display summary statistics
cat("\n Summary statistics for numeric variables:\n")
summary_stats <- summary(jobs_data)
print(summary_stats)

# Check data types
cat("\n Variable types:\n")
var_types <- data.frame(
  Variable = names(jobs_data),
  Type = sapply(jobs_data, class),
  Unique_Values = sapply(jobs_data, function(x) length(unique(x)))
)
print(var_types)

# Save initial data summary
sink("outputs/tables/initial_data_summary.txt")
cat("AI Impact on Jobs 2030 - Initial Data Summary\n")
cat("==============================================\n\n")
cat("Date:", Sys.Date(), "\n")
cat("Dataset Dimensions:", nrow(jobs_data), "×", ncol(jobs_data), "\n\n")
cat("Variable Names:\n")
cat(paste(names(jobs_data), collapse = ", "), "\n\n")
cat("Data Structure:\n")
print(str(jobs_data))
cat("\n Summary Statistics:\n")
print(summary(jobs_data))
sink()

cat("\n Initial data summary saved to: outputs/tables/initial_data_summary.txt\n")
cat("\n Next step: Run 02_data_preparation.R\n")
