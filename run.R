# R Shiny Social Vulnerability Analysis Dashboard
# Entry point for running the application

# Load required packages
required_packages <- c(
  "shiny", "shinydashboard", "DT", "plotly", "ggplot2", "dplyr", 
  "corrplot", "tseries", "leaflet", "sf", "geojsonio", "RColorBrewer",
  "car", "lmtest", "nortest", "moments", "rmarkdown", "knitr", "tinytex"
)

# Install and load packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat("Installing package:", pkg, "\n")
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Create temp_plots directory if it doesn't exist
if (!dir.exists("temp_plots")) {
  dir.create("temp_plots")
  cat("Created temp_plots directory for plot storage\n")
}

# Clean any existing plot files from previous sessions
cat("Cleaning any existing plot files from previous sessions...\n")
cleanup_previous_session <- function() {
  cleaned_files <- 0
  
  # Clean temp_plots directory
  if (dir.exists("temp_plots")) {
    plot_files_temp <- list.files("temp_plots", pattern = "\\.png$", full.names = TRUE)
    if (length(plot_files_temp) > 0) {
      file.remove(plot_files_temp)
      cleaned_files <- cleaned_files + length(plot_files_temp)
    }
  }
  
  # Clean working directory 
  plot_files_parent <- list.files(".", pattern = "^(exploration_|correlation_|normality_|categorization_|regression_|spatial_|anova_).*\\.png$", full.names = TRUE)
  if (length(plot_files_parent) > 0) {
    file.remove(plot_files_parent)
    cleaned_files <- cleaned_files + length(plot_files_parent)
  }
  
  return(cleaned_files)
}

cleaned_count <- cleanup_previous_session()
if (cleaned_count > 0) {
  cat("Cleaned", cleaned_count, "plot files from previous session\n")
}

cat("Starting R Shiny Social Vulnerability Analysis Dashboard...\n")
cat("Dashboard will be available at: http://127.0.0.1:8080\n")

# Run the Shiny application
shiny::runApp(
  appDir = ".",
  port = 8080,
  launch.browser = TRUE,
  host = "127.0.0.1"
)
