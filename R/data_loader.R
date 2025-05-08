# TRCStatR - Data Loading System
# This file handles loading all data from RDS files

#' Load all application data from RDS files
#' 
#' @return A list containing all loaded data frames
load_application_data <- function() {
  # Define paths to RDS files
  data_files <- list(
    players = "tblPlayers.rds",
    teams = "tblTeams.rds",
    batting_stats = "tblBattingStatsFlat.rds",
    pitching_stats = "tblPitchingStatsFlat.rds",
    games = "tblGames.rds",
    divisions = "tblDivisions.rds",
    league_batting_averages = "tblLeagueBattingAverages.rds",
    league_pitching_averages = "tblLeaguePitchingAverages.rds"
  )
  
  # Load and validate each data frame
  loaded_data <- list()
  for (name in names(data_files)) {
    tryCatch({
      file_path <- data_files[[name]]
      if (file.exists(file_path)) {
        loaded_data[[name]] <- readRDS(file_path)

      } else {
        warning(sprintf("RDS file not found: %s", file_path))
        loaded_data[[name]] <- NULL
      }
    }, error = function(e) {
      warning(sprintf("Error loading %s: %s", name, e$message))
      loaded_data[[name]] <- NULL
    })
  }
  
  return(loaded_data)
}

#' Save data to RDS files
#' 
#' @param data A list of data frames to save
#' @param overwrite Whether to overwrite existing files
save_application_data <- function(data, overwrite = TRUE) {
  # Define paths to RDS files
  data_files <- list(
    players = "tblPlayers.rds",
    teams = "tblTeams.rds",
    batting_stats = "tblBattingStatsFlat.rds",
    pitching_stats = "tblPitchingStatsFlat.rds",
    games = "tblGames.rds",
    divisions = "tblDivisions.rds",
    league_batting_averages = "tblLeagueBattingAverages.rds",
    league_pitching_averages = "tblLeaguePitchingAverages.rds"
  )
  
  # Save each data frame
  for (name in names(data)) {
    if (name %in% names(data_files)) {
      file_path <- data_files[[name]]
      if (overwrite || !file.exists(file_path)) {
        tryCatch({
          saveRDS(data[[name]], file_path)

        }, error = function(e) {
          warning(sprintf("Error saving %s: %s", name, e$message))
        })
      }
    }
  }
}
