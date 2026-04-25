# scripts/06_interpretation_report.R
# Step 6: Interpretation Report - Profiles, Explanatory Variables, and Conclusions
# AI Impact on Jobs 2030 Dataset

# Clear environment
rm(list = ls())

# Set working directory
setwd("C:/Users/zahra/OneDrive/Desktop/Project_R")

# Load required libraries
library(tidyverse)
library(knitr)

cat("\n==========================================")
cat("\nStep 6: Interpretation Report")
cat("\n==========================================\n")

# Load all results
cat("\nLoading analysis results...\n")
data_final <- read.csv("data/data_with_clusters.csv")
pca_result <- readRDS("data/pca_result.rds")
kmeans_result <- readRDS("data/kmeans_result.rds")
cluster_profiles <- read.csv("outputs/tables/cluster_profiles.csv")
cluster_categorical <- read.csv("outputs/tables/cluster_categorical_profiles.csv")
anova_results <- read.csv("outputs/tables/anova_f_statistics.csv")
f_stats_df <- anova_results

cat("Results loaded successfully!")

cat("\n\n==================================================================")
cat("\n6.1 CLUSTER PROFILES SUMMARY")
cat("\n==================================================================\n")

# Create comprehensive cluster profiles
cat("\n\n--- CLUSTER 1: COGNITIVE PROFESSIONAL JOBS ---\n")
cat("Size: 1012 observations (33.7% of workforce)\n\n")
cat("Characteristics:\n")
cat("  - High cognitive and analytical skills (Skills 1,3,5,8,9)\n")
cat("  - Moderate AI exposure\n")
cat("  - Lower automation probability\n")
cat("  - Balanced education distribution\n")
cat("  - 50.5% Medium risk, 32.7% Low risk, 16.8% High risk\n\n")
cat("Examples: Knowledge workers, analysts, professionals\n\n")
cat("Risk Level: MEDIUM\n")
cat("Recommended Action: Focus on human-AI collaboration training\n")

cat("\n\n--- CLUSTER 2: HIGH AUTOMATION RISK JOBS ---\n")
cat("Size: 1026 observations (34.2% of workforce)\n\n")
cat("Characteristics:\n")
cat("  - Lower cognitive skill scores\n")
cat("  - Lower years of experience\n")
cat("  - Highest automation probability\n")
cat("  - Highest technology growth factor\n")
cat("  - 50.5% Medium risk, 30.8% High risk, 18.7% Low risk\n\n")
cat("Examples: Routine task workers, entry-level positions\n\n")
cat("Risk Level: HIGH\n")
cat("Recommended Action: Implement reskilling and upskilling programs\n")

cat("\n\n--- CLUSTER 3: TECHNICAL SPECIALIST JOBS ---\n")
cat("Size: 962 observations (32.1% of workforce)\n\n")
cat("Characteristics:\n")
cat("  - Highest years of experience\n")
cat("  - Highest AI exposure index\n")
cat("  - High technical skills (Skills 6,10,2)\n")
cat("  - Lower technology growth factor (stable sectors)\n")
cat("  - 51.1% Medium risk, 26.4% High risk, 22.5% Low risk\n\n")
cat("Examples: Technical specialists, experienced professionals\n\n")
cat("Risk Level: LOW TO MEDIUM\n")
cat("Recommended Action: Preserve specialized skills, monitor AI developments\n")

cat("\n==================================================================")
cat("\n6.2 EXPLANATORY VARIABLES ANALYSIS")
cat("\n==================================================================\n")

cat("\nTop variables differentiating clusters (ANOVA F-statistics):\n")
top_vars <- head(f_stats_df, 10)
for(i in 1:nrow(top_vars)) {
  cat(sprintf("  %d. %s: F = %.2f\n", i, top_vars$Variable[i], top_vars$F_statistic[i]))
}

cat("\nInterpretation of key variables:\n")
cat("\n  SKILL VARIABLES (Primary discriminators):\n")
cat("    - Skill_5 (F=867.92): Strongest differentiator. This skill\n")
cat("      varies dramatically across clusters.\n")
cat("    - Skill_9 (F=354.21): Second most important discriminator.\n")
cat("    - Skill_6 (F=219.63) and Skill_10 (F=158.77): Technical skills\n")
cat("      that separate Cluster 3 from others.\n")
cat("    - Skill_1, Skill_3, Skill_4: Cognitive skills that distinguish\n")
cat("      Cluster 1 (high) from Cluster 2 (low).\n\n")

cat("  EXPERIENCE VARIABLES:\n")
cat("    - Years_Experience (F=134.15): Strong predictor. Cluster 3 has\n")
cat("      highest experience (+0.5 SD), Cluster 2 lowest (-0.5 SD).\n\n")

cat("  AI EXPOSURE VARIABLES:\n")
cat("    - AI_Exposure_Index (F=78.60): Moderate discriminator.\n")
cat("      Highest in Cluster 3 (+0.29 SD).\n")
cat("    - Automation_Probability_2030 (F=52.40): Moderate discriminator.\n")
cat("      Highest in Cluster 2 (+0.19 SD).\n\n")

cat("  ECONOMIC VARIABLES:\n")
cat("    - Average_Salary (F=75.47): Moderate discriminator.\n")
cat("      Highest in Cluster 3 (+0.27 SD).\n")
cat("    - Tech_Growth_Factor (F=13.20): Weak discriminator.\n")

cat("\nNON-DISCRIMINATORS:\n")
cat("  - Education Level: All clusters have nearly identical distributions.\n")
cat("  - Skill_2 and Skill_8: Very low F-statistics, minimal differentiation.\n")

cat("\n==================================================================")
cat("\n6.3 DECISION-ORIENTED CONCLUSIONS")
cat("\n==================================================================\n")

cat("\nCONCLUSION 1: Three Distinct Job Archetypes Exist\n")
cat("  The analysis identifies three meaningful job clusters:\n")
cat("  - Cognitive Professional (33.7%): Low automation risk\n")
cat("  - High Automation Risk (34.2%): Needs intervention\n")
cat("  - Technical Specialist (32.1%): AI-resilient\n")
cat("  This segmentation enables targeted workforce planning.\n")

cat("\nCONCLUSION 2: Skills, Not Education, Determine AI Vulnerability\n")
cat("  Education level does NOT differentiate between clusters.\n")
cat("  Skill profiles are the primary discriminators.\n")
cat("  Implication: Reskilling is more important than degree attainment.\n")

cat("\nCONCLUSION 3: Experience is a Protective Factor\n")
cat("  Years of experience strongly differentiates clusters.\n")
cat("  Cluster 3 (most resilient) has highest experience (+0.5 SD).\n")
cat("  Cluster 2 (most vulnerable) has lowest experience (-0.5 SD).\n")
cat("  Implication: Experienced workers have skills that resist automation.\n")

cat("\nCONCLUSION 4: AI Exposure Alone Does Not Determine Risk\n")
cat("  Cluster 3 has highest AI exposure but remains resilient due to\n")
cat("  complementary technical skills. AI exposure + appropriate skills\n")
cat("  enables augmentation rather than replacement.\n")

cat("\nCONCLUSION 5: The Job Market is Multidimensional\n")
cat("  12 PCA dimensions needed to explain 80% of variance.\n")
cat("  No single 'AI risk factor' dominates.\n")
cat("  Implication: Workforce planning requires multidimensional approach.\n")

cat("\n==================================================================")
cat("\n6.4 IMPACT ON CONTEXT AND DECISION-MAKING")
cat("\n==================================================================\n")

cat("\nORGANIZATIONAL IMPLICATIONS:\n")
cat("\n  For HR and Workforce Planning:\n")
cat("    1. Conduct skill audits to identify which cluster jobs belong to.\n")
cat("    2. Prioritize reskilling for Cluster 2 positions.\n")
cat("    3. Invest in AI collaboration training for Cluster 1.\n")
cat("    4. Preserve and enhance specialized skills in Cluster 3.\n")
cat("    5. Use skill-based hiring rather than degree-based.\n")

cat("\n  For Education and Training Providers:\n")
cat("    1. Focus on developing discriminating skills (Skill_5,9,6,10).\n")
cat("    2. Design programs that combine technical and cognitive skills.\n")
cat("    3. Create pathways for experience building.\n")

cat("\n  For Policy Makers:\n")
cat("    1. Target interventions to Cluster 2 jobs (34.2% of workforce).\n")
cat("    2. Support skill development programs over degree attainment.\n")
cat("    3. Recognize that AI risk is multidimensional.\n")

cat("\n  For Individuals:\n")
cat("    1. Develop discriminating skills identified in this analysis.\n")
cat("    2. Gain experience in specialized domains.\n")
cat("    3. Combine technical skills with cognitive abilities.\n")

cat("\n==================================================================")
cat("\n6.5 RECOMMENDATIONS BY CLUSTER")
cat("\n==================================================================\n")

cat("\nCLUSTER 1 - COGNITIVE PROFESSIONAL JOBS (33.7%):\n")
cat("  Risk Level: MEDIUM\n")
cat("  Recommendations:\n")
cat("    1. Implement human-AI collaboration training programs\n")
cat("    2. Develop AI literacy across the workforce\n")
cat("    3. Identify tasks that can be augmented by AI\n")
cat("    4. Create career paths that leverage AI tools\n")
cat("    5. Invest in continuous learning platforms\n")

cat("\nCLUSTER 2 - HIGH AUTOMATION RISK JOBS (34.2%):\n")
cat("  Risk Level: HIGH\n")
cat("  Recommendations:\n")
cat("    1. Launch immediate reskilling and upskilling programs\n")
cat("    2. Provide transition support to less automatable roles\n")
cat("    3. Identify transferable skills for career pivots\n")
cat("    4. Partner with education providers for targeted training\n")
cat("    5. Implement job rotation programs to build experience\n")

cat("\nCLUSTER 3 - TECHNICAL SPECIALIST JOBS (32.1%):\n")
cat("  Risk Level: LOW TO MEDIUM\n")
cat("  Recommendations:\n")
cat("    1. Preserve and enhance specialized technical skills\n")
cat("    2. Monitor AI developments in technical domains\n")
cat("    3. Create knowledge sharing and mentorship programs\n")
cat("    4. Invest in advanced technical training\n")
cat("    5. Develop AI tools that complement existing expertise\n")

cat("\n==================================================================")
cat("\n6.6 LIMITATIONS AND FUTURE RESEARCH")
cat("\n==================================================================\n")

cat("\nLIMITATIONS:\n")
cat("  1. Data may be synthetic/simulated, limiting real-world generalizability\n")
cat("  2. Low silhouette width (0.131) indicates soft cluster boundaries\n")
cat("  3. 5 PCA dimensions capture only 36% of variance\n")
cat("  4. Results reflect static snapshot, not dynamic changes over time\n")
cat("  5. Skill variables are abstracted as generic Skills 1-10\n\n")

cat("FUTURE RESEARCH DIRECTIONS:\n")
cat("  1. Validate findings with real-world employment data\n")
cat("  2. Track cluster evolution over time as AI develops\n")
cat("  3. Incorporate additional variables (industry, geography, company size)\n")
cat("  4. Develop predictive models for individual job risk assessment\n")
cat("  5. Study intervention effectiveness across clusters\n")

cat("\n==================================================================")
cat("\n6.7 EXECUTIVE SUMMARY")
cat("\n==================================================================\n")

cat("\nKEY FINDINGS:\n")
cat("  - 3,000 jobs analyzed across 15 variables\n")
cat("  - Three distinct job clusters identified\n")
cat("  - Skills are primary discriminators (F up to 867.92)\n")
cat("  - Education does NOT predict AI vulnerability\n")
cat("  - Experience is protective (+0.5 SD in resilient cluster)\n")
cat("  - 12 dimensions needed to explain 80% of variance\n")

cat("\nBOTTOM LINE:\n")
cat("  AI's impact on jobs is multidimensional. Workforce planning must\n")
cat("  focus on skill development rather than degree attainment.\n")
cat("  Approximately 34% of jobs (Cluster 2) face high automation risk\n")
cat("  and require immediate intervention. Cluster 3 demonstrates that\n")
cat("  high AI exposure can coexist with resilience when complemented\n")
cat("  by appropriate technical skills.\n")

cat("\n==================================================================")
cat("\n6.8 SAVE INTERPRETATION REPORTS")
cat("\n==================================================================\n")

# Save comprehensive interpretation report
sink("outputs/tables/final_interpretation_report.txt")

cat("================================================================\n")
cat("FINAL INTERPRETATION REPORT\n")
cat("AI Impact on Jobs 2030 - Multivariate Analysis\n")
cat("================================================================\n\n")

cat("Date:", Sys.Date(), "\n\n")

cat("================================================================\n")
cat("1. EXECUTIVE SUMMARY\n")
cat("================================================================\n\n")

cat("This report presents the results of a comprehensive multivariate\n")
cat("analysis of 3,000 jobs to understand AI's impact on the workforce\n")
cat("by 2030. The analysis employed Principal Component Analysis (PCA)\n")
cat("followed by K-means clustering to identify distinct job archetypes\n")
cat("based on their AI vulnerability profiles.\n\n")

cat("Key findings:\n")
cat("  - Three distinct job clusters were identified\n")
cat("  - Skills, not education, determine AI vulnerability\n")
cat("  - 34.2% of jobs face high automation risk\n")
cat("  - Technical specialists show resilience despite high AI exposure\n")
cat("  - The job market is multidimensional (12 dimensions for 80% variance)\n\n")

cat("================================================================\n")
cat("2. CLUSTER PROFILES\n")
cat("================================================================\n\n")

cat("CLUSTER 1: COGNITIVE PROFESSIONAL JOBS (33.7%)\n")
cat("  Characteristics: High cognitive skills, moderate AI exposure,\n")
cat("  lower automation probability, balanced education.\n")
cat("  Risk: MEDIUM\n")
cat("  Recommendation: Human-AI collaboration training\n\n")

cat("CLUSTER 2: HIGH AUTOMATION RISK JOBS (34.2%)\n")
cat("  Characteristics: Lower cognitive skills, lower experience,\n")
cat("  highest automation probability, high tech growth.\n")
cat("  Risk: HIGH\n")
cat("  Recommendation: Immediate reskilling programs\n\n")

cat("CLUSTER 3: TECHNICAL SPECIALIST JOBS (32.1%)\n")
cat("  Characteristics: Highest experience, highest AI exposure,\n")
cat("  high technical skills, stable tech sectors.\n")
cat("  Risk: LOW TO MEDIUM\n")
cat("  Recommendation: Preserve specialized skills\n\n")

cat("================================================================\n")
cat("3. EXPLANATORY VARIABLES\n")
cat("================================================================\n\n")

cat("Top discriminators (ANOVA F-statistics):\n")
for(i in 1:nrow(top_vars)) {
  cat(sprintf("  %d. %s: F = %.2f\n", i, top_vars$Variable[i], top_vars$F_statistic[i]))
}

cat("\nNon-discriminators: Education Level, Skill_2, Skill_8\n\n")

cat("================================================================\n")
cat("4. PCA RESULTS SUMMARY\n")
cat("================================================================\n\n")

eig_values <- pca_result$eig
cat(sprintf("  - First 2 dimensions explain: %.2f%% of variance\n", sum(eig_values[1:2, 2])))
cat(sprintf("  - First 5 dimensions explain: %.2f%% of variance\n", sum(eig_values[1:5, 2])))
cat(sprintf("  - First 12 dimensions explain: %.2f%% of variance\n", eig_values[12, 3]))
cat("  - Kaiser criterion: 7 dimensions to retain\n")
cat("  - Recommended for interpretation: 5 dimensions\n\n")

cat("Dimension interpretations:\n")
cat(sprintf("  - Dim1 (%.2f%%): Economic & Technology Factor\n", eig_values[1, 2]))
cat(sprintf("  - Dim2 (%.2f%%): Cognitive & Analytical Skills Factor\n", eig_values[2, 2]))
cat(sprintf("  - Dim3 (%.2f%%): AI Exposure & Adaptability Factor\n", eig_values[3, 2]))

cat("\n================================================================\n")
cat("5. CLUSTERING VALIDATION\n")
cat("================================================================\n\n")

cat("  - Average silhouette width: 0.131 (weak but acceptable)\n")
cat("  - Between-cluster distances: ~1.94 units\n")
cat("  - All ANOVA p-values < 0.001\n")
cat("  - Clusters are statistically distinct\n\n")

cat("================================================================\n")
cat("6. CONCLUSIONS AND RECOMMENDATIONS\n")
cat("================================================================\n\n")

cat("For Organizations:\n")
cat("  1. Conduct skill audits to identify cluster membership\n")
cat("  2. Prioritize reskilling for Cluster 2 positions\n")
cat("  3. Invest in AI collaboration training for Cluster 1\n")
cat("  4. Preserve specialized skills in Cluster 3\n")
cat("  5. Use skill-based hiring over degree-based\n\n")

cat("For Policy Makers:\n")
cat("  1. Target interventions to vulnerable jobs (Cluster 2)\n")
cat("  2. Support skill development programs\n")
cat("  3. Recognize multidimensional nature of AI risk\n\n")

cat("For Individuals:\n")
cat("  1. Develop discriminating skills (Skills 5,9,6,10)\n")
cat("  2. Gain specialized experience\n")
cat("  3. Combine technical and cognitive abilities\n\n")

cat("================================================================\n")
cat("7. LIMITATIONS\n")
cat("================================================================\n\n")

cat("  - Data may be synthetic/simulated\n")
cat("  - Low silhouette width indicates soft boundaries\n")
cat("  - Static snapshot, not dynamic over time\n")
cat("  - Limited variable set (15 numeric variables)\n\n")

sink()

cat("Final interpretation report saved to: outputs/tables/final_interpretation_report.txt\n")

# Save recommendations as CSV
recommendations_df <- data.frame(
  Cluster = 1:3,
  Name = c("Cognitive Professional", "High Automation Risk", "Technical Specialist"),
  Size = c("1,012 (33.7%)", "1,026 (34.2%)", "962 (32.1%)"),
  Risk_Level = c("Medium", "High", "Low to Medium"),
  Key_Action = c("AI collaboration training", "Reskilling programs", "Skill preservation")
)

write.csv(recommendations_df, "outputs/tables/cluster_recommendations.csv", row.names = FALSE)

cat("\nCluster recommendations saved to: outputs/tables/cluster_recommendations.csv\n")

# Create summary table for report
summary_table <- data.frame(
  Metric = c("Total observations", "Number of clusters", "PCA dimensions (80% variance)",
             "Silhouette width", "Top discriminator F-statistic", "High risk jobs %"),
  Value = c("3,000", "3", "12", "0.131", "867.92 (Skill_5)", "34.2%")
)

write.csv(summary_table, "outputs/tables/summary_statistics.csv", row.names = FALSE)

cat("\nSummary statistics saved to: outputs/tables/summary_statistics.csv\n")

cat("\n==================================================================")
cat("\nSTEP 6 COMPLETE!")
cat("\n==================================================================\n")

cat("\nFINAL OUTPUTS GENERATED:\n")
cat("  - outputs/tables/final_interpretation_report.txt\n")
cat("  - outputs/tables/cluster_recommendations.csv\n")
cat("  - outputs/tables/summary_statistics.csv\n")
cat("  - All previous outputs from Steps 1-5\n")

cat("\n\n==========================================")
cat("\nPROJECT COMPLETE!")
cat("\n==========================================")
cat("\n")
cat("Summary of all analyses performed:\n")
cat("  Step 1: Problem Understanding & Data Loading\n")
cat("  Step 2: Data Preparation (Missing values, outliers, normalization)\n")
cat("  Step 3: Principal Component Analysis (PCA)\n")
cat("  Step 4: Clustering Analysis (K-means & HAC)\n")
cat("  Step 5: Combined Analysis (PCA + Clustering)\n")
cat("  Step 6: Interpretation Report\n")
cat("\nAll outputs saved in 'outputs/' directory.\n")
cat("R Markdown report can be generated from 'reports/01_project_report.Rmd'\n")