# validation_utils.R
# Module for comprehensive input validation in TRCStatR

# Load error handling module if not already loaded
if (!exists("safe_call")) {
  source("R/error_handling.R")
}

# Declare global variables to avoid lint warnings
utils::globalVariables(c(
  # Functions
  "log_error", "log_warning", "notify_user",
  
  # Data frames and variables
  "teams", "players", "batting_stats", "pitching_stats"
))

#' Validate player data
#'
#' This function validates player data before it's saved to the database.
#'
#' @param player_data List or data frame of player data
#' @param required_fields Character vector of required fields
#' @param log_error Whether to log validation errors
#' @param notify_user Whether to notify the user of validation errors
#' @param session Shiny session object (required if notify_user is TRUE)
#' @return List with validation result (valid) and error messages (errors)
validate_player_data <- function(player_data, required_fields = c("FirstName", "LastInitial", "TeamID"), 
                                log_error = TRUE, notify_user = FALSE, session = NULL) {
  errors <- character()
  
  # Check if player_data is NULL
  if (is.null(player_data)) {
    errors <- c(errors, "Player data is NULL")
    if (log_error) log_error("Player data validation failed: data is NULL")
    return(list(valid = FALSE, errors = errors))
  }
  
  # Check required fields
  for (field in required_fields) {
    if (!field %in% names(player_data) || is.null(player_data[[field]]) || 
        is.na(player_data[[field]]) || player_data[[field]] == "") {
      errors <- c(errors, paste("Missing required field:", field))
    }
  }
  
  # Validate first name (if present)
  if ("FirstName" %in% names(player_data) && !is.null(player_data$FirstName)) {
    if (nchar(player_data$FirstName) < 2) {
      errors <- c(errors, "First name must be at least 2 characters")
    }
    if (nchar(player_data$FirstName) > 50) {
      errors <- c(errors, "First name must be at most 50 characters")
    }
  }
  
  # Validate last initial (if present)
  if ("LastInitial" %in% names(player_data) && !is.null(player_data$LastInitial)) {
    if (nchar(player_data$LastInitial) < 1) {
      errors <- c(errors, "Last initial must be at least 1 character")
    }
    if (nchar(player_data$LastInitial) > 1) {
      errors <- c(errors, "Last initial must be exactly 1 character")
    }
  }
  
  # Validate jersey number (if present)
  if ("JerseyNumber" %in% names(player_data) && !is.null(player_data$JerseyNumber)) {
    if (!is.numeric(player_data$JerseyNumber)) {
      errors <- c(errors, "Jersey number must be a number")
    } else if (player_data$JerseyNumber < 0 || player_data$JerseyNumber > 99) {
      errors <- c(errors, "Jersey number must be between 0 and 99")
    }
  }
  
  # Validate team ID (if present)
  if ("TeamID" %in% names(player_data) && !is.null(player_data$TeamID)) {
    if (!is.numeric(player_data$TeamID)) {
      errors <- c(errors, "Team ID must be a number")
    }
  }
  
  # Log errors if requested
  if (length(errors) > 0 && log_error) {
    log_error(paste("Player data validation failed:", paste(errors, collapse = "; ")))
  }
  
  # Notify user if requested
  if (length(errors) > 0 && notify_user && !is.null(session)) {
    notify_user(paste("Validation errors:", paste(errors, collapse = "; ")), "error", session)
  }
  
  return(list(valid = length(errors) == 0, errors = errors))
}

#' Validate game data
#'
#' This function validates game data before it's saved to the database.
#'
#' @param game_data List or data frame of game data
#' @param required_fields Character vector of required fields
#' @param log_error Whether to log validation errors
#' @param notify_user Whether to notify the user of validation errors
#' @param session Shiny session object (required if notify_user is TRUE)
#' @return List with validation result (valid) and error messages (errors)
validate_game_data <- function(game_data, required_fields = c("HomeTeamID", "AwayTeamID", "GameDate"), 
                              log_error = TRUE, notify_user = FALSE, session = NULL) {
  errors <- character()
  
  # Check if game_data is NULL
  if (is.null(game_data)) {
    errors <- c(errors, "Game data is NULL")
    if (log_error) log_error("Game data validation failed: data is NULL")
    return(list(valid = FALSE, errors = errors))
  }
  
  # Check required fields
  for (field in required_fields) {
    if (!field %in% names(game_data) || is.null(game_data[[field]]) || 
        is.na(game_data[[field]]) || game_data[[field]] == "") {
      errors <- c(errors, paste("Missing required field:", field))
    }
  }
  
  # Validate home team ID (if present)
  if ("HomeTeamID" %in% names(game_data) && !is.null(game_data$HomeTeamID)) {
    if (!is.numeric(game_data$HomeTeamID)) {
      errors <- c(errors, "Home team ID must be a number")
    }
  }
  
  # Validate away team ID (if present)
  if ("AwayTeamID" %in% names(game_data) && !is.null(game_data$AwayTeamID)) {
    if (!is.numeric(game_data$AwayTeamID)) {
      errors <- c(errors, "Away team ID must be a number")
    }
  }
  
  # Validate game date (if present)
  if ("GameDate" %in% names(game_data) && !is.null(game_data$GameDate)) {
    if (!inherits(game_data$GameDate, "Date") && !is.character(game_data$GameDate)) {
      errors <- c(errors, "Game date must be a date or character string")
    } else if (is.character(game_data$GameDate)) {
      # Try to parse the date
      tryCatch({
        as.Date(game_data$GameDate)
      }, error = function(e) {
        errors <<- c(errors, "Invalid game date format")
      })
    }
  }
  
  # Validate that home and away teams are different
  if ("HomeTeamID" %in% names(game_data) && "AwayTeamID" %in% names(game_data) &&
      !is.null(game_data$HomeTeamID) && !is.null(game_data$AwayTeamID)) {
    if (game_data$HomeTeamID == game_data$AwayTeamID) {
      errors <- c(errors, "Home and away teams must be different")
    }
  }
  
  # Log errors if requested
  if (length(errors) > 0 && log_error) {
    log_error(paste("Game data validation failed:", paste(errors, collapse = "; ")))
  }
  
  # Notify user if requested
  if (length(errors) > 0 && notify_user && !is.null(session)) {
    notify_user(paste("Validation errors:", paste(errors, collapse = "; ")), "error", session)
  }
  
  return(list(valid = length(errors) == 0, errors = errors))
}

#' Validate batting statistics
#'
#' This function validates batting statistics before they're saved to the database.
#'
#' @param batting_data List or data frame of batting statistics
#' @param required_fields Character vector of required fields
#' @param log_error Whether to log validation errors
#' @param notify_user Whether to notify the user of validation errors
#' @param session Shiny session object (required if notify_user is TRUE)
#' @return List with validation result (valid) and error messages (errors)
validate_batting_stats <- function(batting_data, required_fields = c("PlayerID", "GameID", "PA"), 
                                  log_error = TRUE, notify_user = FALSE, session = NULL) {
  errors <- character()
  
  # Check if batting_data is NULL
  if (is.null(batting_data)) {
    errors <- c(errors, "Batting data is NULL")
    if (log_error) log_error("Batting stats validation failed: data is NULL")
    return(list(valid = FALSE, errors = errors))
  }
  
  # Check required fields
  for (field in required_fields) {
    if (!field %in% names(batting_data) || is.null(batting_data[[field]]) || 
        is.na(batting_data[[field]])) {
      errors <- c(errors, paste("Missing required field:", field))
    }
  }
  
  # Validate player ID (if present)
  if ("PlayerID" %in% names(batting_data) && !is.null(batting_data$PlayerID)) {
    if (!is.numeric(batting_data$PlayerID)) {
      errors <- c(errors, "Player ID must be a number")
    }
  }
  
  # Validate game ID (if present)
  if ("GameID" %in% names(batting_data) && !is.null(batting_data$GameID)) {
    if (!is.numeric(batting_data$GameID)) {
      errors <- c(errors, "Game ID must be a number")
    }
  }
  
  # Validate plate appearances (if present)
  if ("PA" %in% names(batting_data) && !is.null(batting_data$PA)) {
    if (!is.numeric(batting_data$PA)) {
      errors <- c(errors, "Plate appearances must be a number")
    } else if (batting_data$PA < 0) {
      errors <- c(errors, "Plate appearances cannot be negative")
    }
  }
  
  # Validate consistency of batting stats
  numeric_fields <- c("PA", "AB", "X1B", "X2B", "X3B", "HR", "BB", "HBP", "SF", "SH", "SB", "RBI", "K")
  for (field in numeric_fields) {
    if (field %in% names(batting_data) && !is.null(batting_data[[field]])) {
      if (!is.numeric(batting_data[[field]])) {
        errors <- c(errors, paste(field, "must be a number"))
      } else if (batting_data[[field]] < 0) {
        errors <- c(errors, paste(field, "cannot be negative"))
      }
    }
  }
  
  # Check that AB <= PA
  if (all(c("PA", "AB") %in% names(batting_data)) && 
      !is.null(batting_data$PA) && !is.null(batting_data$AB)) {
    if (batting_data$AB > batting_data$PA) {
      errors <- c(errors, "At-bats cannot exceed plate appearances")
    }
  }
  
  # Check if the sum of hits is consistent with AB
  if (all(c("AB", "X1B", "X2B", "X3B", "HR", "K") %in% names(batting_data)) &&
      !is.null(batting_data$AB) && !is.null(batting_data$X1B) && 
      !is.null(batting_data$X2B) && !is.null(batting_data$X3B) && 
      !is.null(batting_data$HR) && !is.null(batting_data$K)) {
    
    total_hits <- batting_data$X1B + batting_data$X2B + batting_data$X3B + batting_data$HR
    if (total_hits > batting_data$AB) {
      errors <- c(errors, "Total hits cannot exceed at-bats")
    }
    
    if (total_hits + batting_data$K > batting_data$AB) {
      errors <- c(errors, "Total hits plus strikeouts cannot exceed at-bats")
    }
  }
  
  # Log errors if requested
  if (length(errors) > 0 && log_error) {
    log_error(paste("Batting stats validation failed:", paste(errors, collapse = "; ")))
  }
  
  # Notify user if requested
  if (length(errors) > 0 && notify_user && !is.null(session)) {
    notify_user(paste("Validation errors:", paste(errors, collapse = "; ")), "error", session)
  }
  
  return(list(valid = length(errors) == 0, errors = errors))
}

#' Validate pitching statistics
#'
#' This function validates pitching statistics before they're saved to the database.
#'
#' @param pitching_data List or data frame of pitching statistics
#' @param required_fields Character vector of required fields
#' @param log_error Whether to log validation errors
#' @param notify_user Whether to notify the user of validation errors
#' @param session Shiny session object (required if notify_user is TRUE)
#' @return List with validation result (valid) and error messages (errors)
validate_pitching_stats <- function(pitching_data, required_fields = c("PlayerID", "GameID", "OR"), 
                                   log_error = TRUE, notify_user = FALSE, session = NULL) {
  errors <- character()
  
  # Check if pitching_data is NULL
  if (is.null(pitching_data)) {
    errors <- c(errors, "Pitching data is NULL")
    if (log_error) log_error("Pitching stats validation failed: data is NULL")
    return(list(valid = FALSE, errors = errors))
  }
  
  # Check required fields
  for (field in required_fields) {
    if (!field %in% names(pitching_data) || is.null(pitching_data[[field]]) || 
        is.na(pitching_data[[field]])) {
      errors <- c(errors, paste("Missing required field:", field))
    }
  }
  
  # Validate player ID (if present)
  if ("PlayerID" %in% names(pitching_data) && !is.null(pitching_data$PlayerID)) {
    if (!is.numeric(pitching_data$PlayerID)) {
      errors <- c(errors, "Player ID must be a number")
    }
  }
  
  # Validate game ID (if present)
  if ("GameID" %in% names(pitching_data) && !is.null(pitching_data$GameID)) {
    if (!is.numeric(pitching_data$GameID)) {
      errors <- c(errors, "Game ID must be a number")
    }
  }
  
  # Validate outs recorded (if present)
  if ("OR" %in% names(pitching_data) && !is.null(pitching_data$OR)) {
    if (!is.numeric(pitching_data$OR)) {
      errors <- c(errors, "Outs recorded must be a number")
    } else if (pitching_data$OR < 0) {
      errors <- c(errors, "Outs recorded cannot be negative")
    }
  }
  
  # Validate consistency of pitching stats
  numeric_fields <- c("OR", "H", "R", "ER", "BB", "K", "HR", "HBP")
  for (field in numeric_fields) {
    if (field %in% names(pitching_data) && !is.null(pitching_data[[field]])) {
      if (!is.numeric(pitching_data[[field]])) {
        errors <- c(errors, paste(field, "must be a number"))
      } else if (pitching_data[[field]] < 0) {
        errors <- c(errors, paste(field, "cannot be negative"))
      }
    }
  }
  
  # Check that ER <= R
  if (all(c("R", "ER") %in% names(pitching_data)) && 
      !is.null(pitching_data$R) && !is.null(pitching_data$ER)) {
    if (pitching_data$ER > pitching_data$R) {
      errors <- c(errors, "Earned runs cannot exceed total runs")
    }
  }
  
  # Log errors if requested
  if (length(errors) > 0 && log_error) {
    log_error(paste("Pitching stats validation failed:", paste(errors, collapse = "; ")))
  }
  
  # Notify user if requested
  if (length(errors) > 0 && notify_user && !is.null(session)) {
    notify_user(paste("Validation errors:", paste(errors, collapse = "; ")), "error", session)
  }
  
  return(list(valid = length(errors) == 0, errors = errors))
}

#' Validate team data
#'
#' This function validates team data before it's saved to the database.
#'
#' @param team_data List or data frame of team data
#' @param required_fields Character vector of required fields
#' @param log_error Whether to log validation errors
#' @param notify_user Whether to notify the user of validation errors
#' @param session Shiny session object (required if notify_user is TRUE)
#' @return List with validation result (valid) and error messages (errors)
validate_team_data <- function(team_data, required_fields = c("TeamName", "DivisionID"), 
                              log_error = TRUE, notify_user = FALSE, session = NULL) {
  errors <- character()
  
  # Check if team_data is NULL
  if (is.null(team_data)) {
    errors <- c(errors, "Team data is NULL")
    if (log_error) log_error("Team data validation failed: data is NULL")
    return(list(valid = FALSE, errors = errors))
  }
  
  # Check required fields
  for (field in required_fields) {
    if (!field %in% names(team_data) || is.null(team_data[[field]]) || 
        is.na(team_data[[field]]) || team_data[[field]] == "") {
      errors <- c(errors, paste("Missing required field:", field))
    }
  }
  
  # Validate team name (if present)
  if ("TeamName" %in% names(team_data) && !is.null(team_data$TeamName)) {
    if (nchar(team_data$TeamName) < 2) {
      errors <- c(errors, "Team name must be at least 2 characters")
    }
    if (nchar(team_data$TeamName) > 50) {
      errors <- c(errors, "Team name must be at most 50 characters")
    }
  }
  
  # Validate division ID (if present)
  if ("DivisionID" %in% names(team_data) && !is.null(team_data$DivisionID)) {
    if (!is.numeric(team_data$DivisionID)) {
      errors <- c(errors, "Division ID must be a number")
    }
  }
  
  # Log errors if requested
  if (length(errors) > 0 && log_error) {
    log_error(paste("Team data validation failed:", paste(errors, collapse = "; ")))
  }
  
  # Notify user if requested
  if (length(errors) > 0 && notify_user && !is.null(session)) {
    notify_user(paste("Validation errors:", paste(errors, collapse = "; ")), "error", session)
  }
  
  return(list(valid = length(errors) == 0, errors = errors))
}

#' Sanitize input for database
#'
#' This function sanitizes input data for database operations.
#'
#' @param data List or data frame of data to sanitize
#' @param numeric_fields Character vector of fields that should be numeric
#' @param character_fields Character vector of fields that should be character
#' @param date_fields Character vector of fields that should be dates
#' @param boolean_fields Character vector of fields that should be boolean
#' @return Sanitized data
sanitize_input <- function(data, numeric_fields = character(), character_fields = character(),
                          date_fields = character(), boolean_fields = character()) {
  # Make a copy of the data to avoid modifying the original
  sanitized <- data
  
  # Sanitize numeric fields
  for (field in numeric_fields) {
    if (field %in% names(sanitized)) {
      if (is.null(sanitized[[field]]) || is.na(sanitized[[field]])) {
        sanitized[[field]] <- 0
      } else {
        sanitized[[field]] <- as.numeric(sanitized[[field]])
        if (is.na(sanitized[[field]])) {
          sanitized[[field]] <- 0
        }
      }
    }
  }
  
  # Sanitize character fields
  for (field in character_fields) {
    if (field %in% names(sanitized)) {
      if (is.null(sanitized[[field]]) || is.na(sanitized[[field]])) {
        sanitized[[field]] <- ""
      } else {
        sanitized[[field]] <- as.character(sanitized[[field]])
        # Escape single quotes for SQL
        sanitized[[field]] <- gsub("'", "''", sanitized[[field]])
      }
    }
  }
  
  # Sanitize date fields
  for (field in date_fields) {
    if (field %in% names(sanitized)) {
      if (is.null(sanitized[[field]]) || is.na(sanitized[[field]])) {
        sanitized[[field]] <- as.Date(NA)
      } else if (!inherits(sanitized[[field]], "Date")) {
        # Try to convert to date
        tryCatch({
          sanitized[[field]] <- as.Date(sanitized[[field]])
        }, error = function(e) {
          sanitized[[field]] <- as.Date(NA)
        })
      }
    }
  }
  
  # Sanitize boolean fields
  for (field in boolean_fields) {
    if (field %in% names(sanitized)) {
      if (is.null(sanitized[[field]]) || is.na(sanitized[[field]])) {
        sanitized[[field]] <- FALSE
      } else {
        sanitized[[field]] <- as.logical(sanitized[[field]])
        if (is.na(sanitized[[field]])) {
          sanitized[[field]] <- FALSE
        }
      }
    }
  }
  
  return(sanitized)
}
