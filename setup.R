# Set your project path
project_root <- "C:/Users/zahra/OneDrive/Desktop/Project_R"

# Create directories
dir.create(file.path(project_root, "data"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(project_root, "scripts"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(project_root, "outputs/figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(project_root, "outputs/tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(project_root, "reports"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(project_root, "presentation"), recursive = TRUE, showWarnings = FALSE)

# Create empty script files
file.create(file.path(project_root, "scripts", "01_load_data.R"))
file.create(file.path(project_root, "scripts", "02_data_preparation.R"))
file.create(file.path(project_root, "scripts", "03_factor_analysis_PCA.R"))
file.create(file.path(project_root, "scripts", "04_clustering_analysis.R"))
file.create(file.path(project_root, "scripts", "05_combined_analysis.R"))
file.create(file.path(project_root, "scripts", "06_interpretation_report.R"))
file.create(file.path(project_root, "scripts", "functions.R"))

# Check if everything was created
list.files(file.path(project_root, "scripts"))
list.dirs(project_root)