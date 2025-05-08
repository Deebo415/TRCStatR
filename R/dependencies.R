# dependencies.R
# Module for managing dependencies in TRCStatR

#' Check and install required packages
#'
#' This function checks if required packages are installed and installs them if needed.
#' It's designed to be called at application startup.
#'
#' @param packages Character vector of package names to check
#' @param quietly Whether to suppress messages during installation
#' @return Logical vector indicating which packages were newly installed
check_and_install_packages <- function(packages, quietly = FALSE) {
  # Create a vector to track which packages were installed
  installed <- logical(length(packages))
  names(installed) <- packages
  
  # Check each package
  for (i in seq_along(packages)) {
    pkg <- packages[i]
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (!quietly) {
        message(paste("Installing package:", pkg))
      }
      
      # Try to install the package
      install_result <- try(utils::install.packages(pkg, quiet = quietly, dependencies = TRUE), silent = quietly)
      
      # Check if installation was successful
      if (!inherits(install_result, "try-error") && requireNamespace(pkg, quietly = TRUE)) {
        installed[i] <- TRUE
        if (!quietly) {
          message(paste("Successfully installed package:", pkg))
        }
      } else {
        if (!quietly) {
          warning(paste("Failed to install package:", pkg))
        }
      }
    }
  }
  
  return(installed)
}

#' Load required packages
#'
#' This function loads all required packages for TRCStatR.
#' It first checks if they are installed and installs them if needed.
#'
#' @param quietly Whether to suppress messages during loading
#' @return Invisible NULL
load_required_packages <- function(quietly = FALSE) {
  # Define required packages
  required_packages <- c(
    # Core packages
    "shiny", "shinydashboard", "shinyjs", "DT", "dplyr",
    "shinyWidgets", "ggplot2", "scales",
    
    # Performance packages
    "data.table", "bit64", "future", "future.apply",
    
    # Utility packages
    "logger", "parallel", "DBI"
  )
  
  # Check and install missing packages
  check_and_install_packages(required_packages, quietly)
  
  # Load all packages
  for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      if (!quietly) {
        message(paste("Loading package:", pkg))
      }
      
      # Load the package
      library(pkg, character.only = TRUE, quietly = quietly)
    } else {
      warning(paste("Could not load package:", pkg))
    }
  }
  
  invisible(NULL)
}

#' Initialize all dependencies
#'
#' This function initializes all dependencies for TRCStatR.
#' It's designed to be called at application startup.
#'
#' @param quietly Whether to suppress messages
#' @return Invisible NULL
initialize_dependencies <- function(quietly = FALSE) {
  # Load required packages
  load_required_packages(quietly)
  
  # Initialize other dependencies as needed
  if (!quietly) {
    message("All dependencies initialized successfully")
  }
  
  invisible(NULL)
}
