# Handler for cleaning up resources when connection is lost
# This will be called by the session$onSessionEnded hook

library(shiny)

# This function will be called when the Shiny session ends
# (either normally or due to connection issues)
cleanup_on_session_end <- function() {
  # This function should be called when the session ends
  cat("\n🔌 Session ended - running cleanup...\n")
  
  # Only proceed with cleanup if temp_plots directory exists
  if (dir.exists("temp_plots")) {
    # List all plot files in temp_plots
    plot_files <- list.files("temp_plots", pattern = "\\.png$", full.names = TRUE)
    
    if (length(plot_files) > 0) {
      cat("🧹 Found", length(plot_files), "plot files to clean up\n")
      
      # Try to remove all plot files
      removed <- file.remove(plot_files)
      cat("✅ Removed", sum(removed), "plot files from temp_plots/\n")
    } else {
      cat("ℹ️ No plot files found in temp_plots/\n")
    }
    
    # Also check working directory for any temp plots
    work_dir_plots <- list.files(".", pattern = "^(categorization_|exploration_|correlation_|normality_|regression_|spatial_|anova_).*\\.png$", full.names = TRUE)
    
    if (length(work_dir_plots) > 0) {
      cat("🧹 Found", length(work_dir_plots), "plot files in working directory to clean up\n")
      
      # Try to remove all working directory plot files
      removed_work <- file.remove(work_dir_plots)
      cat("✅ Removed", sum(removed_work), "plot files from working directory\n")
    } else {
      cat("ℹ️ No plot files found in working directory\n")
    }
  } else {
    cat("ℹ️ No temp_plots directory found\n")
  }
  
  cat("✅ Session cleanup completed\n")
}
