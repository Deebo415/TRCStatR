# deploy_to_github.R
# This script prepares the TRCStatR app for GitHub deployment
# and provides instructions for pushing to GitHub

# Load required libraries
library(shiny)
library(jsonlite)
library(htmltools)

# Source the update widgets script to ensure everything is up to date
source("scripts/update_widgets.R")

# Function to check if git is installed
check_git_installed <- function() {
  git_check <- try(system("git --version", intern = TRUE), silent = TRUE)
  if (inherits(git_check, "try-error")) {
    message("Git does not appear to be installed or is not in your PATH.")
    message("Please install Git from https://git-scm.com/downloads")
    return(FALSE)
  }
  return(TRUE)
}

# Function to initialize git repository if not already initialized
init_git_repo <- function() {
  if (!dir.exists(".git")) {
    message("Initializing Git repository...")
    system("git init")
    message("Git repository initialized.")
  } else {
    message("Git repository already initialized.")
  }
}

# Function to create .gitignore if it doesn't exist
create_gitignore <- function() {
  if (!file.exists(".gitignore")) {
    message("Creating .gitignore file...")
    gitignore_content <- c(
      "# R specific",
      ".Rproj.user",
      ".Rhistory",
      ".RData",
      ".Ruserdata",
      "*.Rproj",
      "",
      "# Shiny token",
      "rsconnect/",
      ".httr-oauth",
      "",
      "# OS specific",
      ".DS_Store",
      "Thumbs.db",
      "",
      "# Editor specific",
      ".vscode/",
      ".idea/",
      "",
      "# Local configuration",
      "config.local.R",
      "",
      "# Database files",
      "*.accdb",
      "*.mdb",
      "*.ldb",
      "*.sqlite",
      "*.db"
    )
    writeLines(gitignore_content, ".gitignore")
    message(".gitignore file created.")
  } else {
    message(".gitignore file already exists.")
  }
}

# Function to print deployment instructions
print_deployment_instructions <- function(username = "YOUR-USERNAME") {
  cat("\n=== GITHUB DEPLOYMENT INSTRUCTIONS ===\n\n")
  cat("1. Create a GitHub repository named 'TRCStatR' at https://github.com/new\n\n")
  cat("2. Run these commands in your terminal/command prompt:\n")
  cat("   cd ", getwd(), "\n", sep = "")
  cat("   git add .\n")
  cat("   git commit -m \"Initial commit\"\n")
  cat("   git remote add origin https://github.com/", username, "/TRCStatR.git\n", sep = "")
  cat("   git push -u origin main\n\n")
  cat("3. Enable GitHub Pages in the repository settings:\n")
  cat("   - Go to Settings > Pages\n")
  cat("   - Set the source to 'main' branch\n")
  cat("   - Click Save\n\n")
  cat("4. Your widgets will be available at:\n")
  cat("   https://", username, ".github.io/TRCStatR/widgets/standings/index.html\n\n", sep = "")
  cat("5. To embed the widget on the Rec Council's website, use this code:\n")
  cat("   <iframe src=\"https://", username, ".github.io/TRCStatR/widgets/standings/index.html\" ", sep = "")
  cat("width=\"100%\" height=\"600px\" frameborder=\"0\" scrolling=\"no\"></iframe>\n\n")
  cat("6. To update the widget in the future:\n")
  cat("   - Run source(\"scripts/update_widgets.R\") to update the widget files\n")
  cat("   - Commit and push the changes to GitHub\n")
  cat("   - The widget will automatically update on the Rec Council's website\n")
  cat("==========================================\n")
}

# Main deployment function
deploy_to_github <- function() {
  message("Preparing TRCStatR for GitHub deployment...")
  
  # Check if git is installed
  if (!check_git_installed()) {
    return(invisible(FALSE))
  }
  
  # Initialize git repository
  init_git_repo()
  
  # Create .gitignore
  create_gitignore()
  
  # Ask for GitHub username
  cat("\nEnter your GitHub username (or press Enter to use 'YOUR-USERNAME'): ")
  username <- readline()
  if (username == "") {
    username <- "YOUR-USERNAME"
  }
  
  # Print deployment instructions
  print_deployment_instructions(username)
  
  message("Deployment preparation complete!")
  return(invisible(TRUE))
}

# Run the deployment function
deploy_to_github()
