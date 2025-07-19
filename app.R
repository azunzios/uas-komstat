#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Setup directory structure
source("setup_plot_directories.R")

# Source session cleanup handler for global access
source("session_cleanup.R")

# Source UI and Server
source("ui.R")
source("server.R")

# Run the application 
shinyApp(ui = ui, server = server)