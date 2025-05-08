# refresh_standings.R
# This script ensures that master_team_standings is properly updated
# Run this script before updating the standings widget

# Load required libraries
library(shiny)
library(dplyr)

# Source the object_engine.R file to ensure we have access to all functions
source("R/object_engine.R")

# Function to refresh the master_team_standings table
refresh_master_standings <- function() {
  message("Refreshing master_team_standings...")
  
  # Check if create_master_tables function exists
  if (!exists("create_master_tables", envir = .GlobalEnv)) {
    stop("create_master_tables function not found. Make sure object_engine.R is properly loaded.")
  }
  
  # Call create_master_tables to regenerate all master tables including standings
  create_master_tables()
  
  # Verify that master_team_standings was created
  if (!exists("master_team_standings", envir = .GlobalEnv)) {
    stop("Failed to create master_team_standings.")
  }
  
  # Print the current standings for verification
  standings <- get("master_team_standings", envir = .GlobalEnv)
  if (nrow(standings) > 0) {
    message("Current standings:")
    print(data.frame(
      Team = standings$Team,
      W = standings$W,
      L = standings$L,
      T = standings$T,
      PCT = standings$PCT,
      RS = standings$RS,
      RA = standings$RA,
      RD = standings$RD
    ))
    message("Master standings refreshed successfully with ", nrow(standings), " teams.")
    return(TRUE)
  } else {
    message("Warning: master_team_standings was created but contains no data.")
    return(FALSE)
  }
}

# Run the refresh function
refresh_master_standings()
