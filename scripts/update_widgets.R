# update_widgets.R
# This script updates all widgets and prepares them for GitHub Pages

# Load required libraries
library(shiny)
library(htmltools)
library(jsonlite)
library(dplyr)

# Source the necessary files
source("refresh_standings.R")
source("league_standings_widget.R")

# Create necessary directories
dir.create("widgets/standings/logos", showWarnings = FALSE, recursive = TRUE)

# Function to update the standings widget
update_standings_widget <- function() {
  message("Updating standings widget...")
  
  # Export the current standings data to JSON
  tryCatch({
    # First export to the www directory for the Shiny app
    export_standings_data("www/standings_data.json")
    
    # Then export to the widgets directory for GitHub Pages
    json_path <- export_standings_data("widgets/standings/data.json")
    cat("✓ Standings data exported to", json_path, "\n")
  }, error = function(e) {
    cat("✗ Error exporting standings data:", e$message, "\n")
    stop(e)
  })
  
  # Copy team logos to the widgets directory
  logo_files <- list.files("www/logos", pattern = "\\.png$", full.names = TRUE)
  if (length(logo_files) == 0) {
    # If no logos in www/logos, check the main www directory
    logo_files <- list.files("www", pattern = "\\.png$", full.names = TRUE)
  }
  
  for (logo_file in logo_files) {
    file_name <- basename(logo_file)
    target_path <- file.path("widgets/standings/logos", file_name)
    if (!file.exists(target_path)) {
      file.copy(logo_file, target_path)
      cat("✓ Copied logo:", file_name, "\n")
    }
  }
  
  message("Standings widget updated successfully!")
}

# Function to prepare for GitHub Pages deployment
prepare_for_github_pages <- function() {
  message("Preparing for GitHub Pages deployment...")
  
  # Create a README for the widgets directory
  readme_content <- "# TRCStatR Widgets\n\nThis directory contains standalone widgets that can be embedded in external websites.\n\n## Standings Widget\n\nThe standings widget displays the current team standings and automatically updates when the standings change.\n\nTo use the widget, embed it using an iframe:\n\n```html\n<iframe src=\"https://your-username.github.io/TRCStatR/widgets/standings/index.html\" width=\"100%\" height=\"600px\" frameborder=\"0\" scrolling=\"no\"></iframe>\n```\n\nReplace `your-username` with your actual GitHub username."
  writeLines(readme_content, "widgets/README.md")
  
  message("GitHub Pages preparation complete!")
}

# Run the update functions
update_standings_widget()
prepare_for_github_pages()

# Print instructions for GitHub deployment
cat("\n=== GITHUB DEPLOYMENT INSTRUCTIONS ===\n")
cat("1. Create a GitHub repository named 'TRCStatR'\n")
cat("2. Push your code to the repository:\n")
cat("   git init\n")
cat("   git add .\n")
cat("   git commit -m \"Initial commit\"\n")
cat("   git remote add origin https://github.com/YOUR-USERNAME/TRCStatR.git\n")
cat("   git push -u origin main\n\n")
cat("3. Enable GitHub Pages in the repository settings:\n")
cat("   - Go to Settings > Pages\n")
cat("   - Set the source to 'main' branch\n")
cat("   - Click Save\n\n")
cat("4. Your widgets will be available at:\n")
cat("   https://YOUR-USERNAME.github.io/TRCStatR/widgets/standings/index.html\n")
cat("==========================================\n")

# Create a requirements.txt file for package dependencies
cat("\nCreating requirements.txt file for package dependencies...\n")
requirements <- c(
  "shiny",
  "shinydashboard",
  "DT",
  "dplyr",
  "ggplot2",
  "htmltools",
  "jsonlite",
  "htmlwidgets"
)
writeLines(requirements, "requirements.txt")
cat("✓ Created requirements.txt\n")
