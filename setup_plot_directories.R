# This script verifies that the temp_plots directory exists and is accessible
# It will be automatically sourced at startup to ensure proper structure

# Ensure temp_plots directory exists
setup_plot_directories <- function() {
  # Create temp_plots directory if it doesn't exist
  if (!dir.exists("temp_plots")) {
    dir.create("temp_plots")
    cat("Created temp_plots directory for plot storage\n")
  }
  
  # Ensure directory has write permissions
  if (file.access("temp_plots", 2) != 0) {
    # Try to fix permissions
    Sys.chmod("temp_plots", mode = "0755")
    cat("Updated temp_plots directory permissions\n")
  }
  
  # Create a test file to verify write access
  test_file <- file.path("temp_plots", "test_write.txt")
  tryCatch({
    writeLines("Plot directory access test", test_file)
    file.remove(test_file)
    cat("Verified write access to temp_plots directory\n")
  }, error = function(e) {
    cat("WARNING: Cannot write to temp_plots directory. Error:", e$message, "\n")
  })
  
  return(TRUE)
}

setup_plot_directories()

# Define a centralized function for saving plots with proper error handling
save_plot_with_verification <- function(filename, plot_func, width = 1000, height = 500, res = 100) {
  # Ensure temp_plots directory exists
  if (!dir.exists("temp_plots")) {
    dir.create("temp_plots")
  }
  
  # Create full path
  plot_path <- file.path("temp_plots", filename)
  
  # Save plot with error handling
  result <- tryCatch({
    png(plot_path, width = width, height = height, res = res)
    plot_func()
    dev.off()
    
    # Verify file exists and has content
    if (file.exists(plot_path) && file.info(plot_path)$size > 0) {
      # Make a copy to working directory for LaTeX access
      file.copy(plot_path, filename, overwrite = TRUE)
      if (file.exists(filename)) {
        Sys.chmod(filename, mode = "0644")
        return(list(success = TRUE, path = plot_path, filename = filename))
      } else {
        return(list(success = FALSE, error = "Failed to copy plot to working directory"))
      }
    } else {
      return(list(success = FALSE, error = "Plot file missing or empty"))
    }
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
  
  return(result)
}

# For use in the app, attach to global environment
assign("save_plot_with_verification", save_plot_with_verification, envir = .GlobalEnv)
