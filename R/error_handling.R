# error_handling.R
# Module for consistent error handling in TRCStatR

library(shiny)
library(logger)

# Initialize logger
log_appender(appender_file("logs/trcstatr.log"))
log_layout(layout_glue_generator(
  format = "{level} [{time}] {msg}"
))

#' Initialize error handling system
#' 
#' Sets up the error handling system with proper logging configuration
#' 
#' @param log_level The logging level (default: "INFO")
#' @param log_file The log file path (default: "logs/trcstatr.log")
#' @return NULL
initialize_error_handling <- function(log_level = "INFO", log_file = "logs/trcstatr.log") {
  # Create logs directory if it doesn't exist
  dir.create("logs", showWarnings = FALSE)
  
  # Set log level
  log_threshold(log_level)
  
  # Configure file appender
  log_appender(appender_file(log_file))
  
  # Set layout
  log_layout(layout_glue_generator(
    format = "{level} [{time}] {msg}"
  ))
  
  # Log initialization
  log_info("Error handling system initialized")
}

#' Handle errors consistently
#' 
#' This function provides a consistent way to handle errors across the application
#' 
#' @param expr The expression to evaluate
#' @param error_message A custom error message
#' @param default_value The default value to return on error
#' @param log_error Whether to log the error (default: TRUE)
#' @param notify_user Whether to notify the user (default: FALSE)
#' @param session The Shiny session object (required if notify_user is TRUE)
#' @return The result of the expression or the default value on error
safe_call <- function(expr, error_message = NULL, default_value = NULL, 
                      log_error = TRUE, notify_user = FALSE, session = NULL) {
  tryCatch({
    expr
  }, error = function(e) {
    # Create error message
    msg <- if (!is.null(error_message)) {
      paste0(error_message, ": ", e$message)
    } else {
      e$message
    }
    
    # Log error
    if (log_error) {
      log_error(msg)
    }
    
    # Notify user
    if (notify_user && !is.null(session)) {
      session$sendCustomMessage("notification", 
                               list(type = "error", 
                                    message = paste("Error:", msg)))
    }
    
    # Return default value
    default_value
  }, warning = function(w) {
    # Log warning
    if (log_error) {
      log_warn(w$message)
    }
    
    # Continue with expression evaluation
    expr
  })
}

#' Handle database errors consistently
#' 
#' This function provides a consistent way to handle database errors
#' 
#' @param query_expr The database query expression to evaluate
#' Display a user notification
#' 
#' This function displays a notification to the user
#' 
#' @param message The message to display
#' @param type The type of notification (default: "info")
#' @param session The Shiny session object
#' @return NULL
notify_user <- function(message, type = "info", session = getDefaultReactiveDomain()) {
  if (!is.null(session)) {
    session$sendCustomMessage("notification", 
                             list(type = type, 
                                  message = message))
  }
}

#' Log and notify about an error
#' 
#' This function logs an error and notifies the user
#' 
#' @param message The error message
#' @param session The Shiny session object
#' @return NULL
log_and_notify <- function(message, session = getDefaultReactiveDomain()) {
  log_error(message)
  notify_user(message, type = "error", session = session)
}

#' Validate input data
#' 
#' This function validates input data and returns TRUE if valid, FALSE otherwise
#' 
#' @param data The data to validate
#' @param rules A list of validation rules
#' @param error_message A custom error message
#' @param log_error Whether to log the error (default: TRUE)
#' @param notify_user Whether to notify the user (default: FALSE)
#' @param session The Shiny session object (required if notify_user is TRUE)
#' @return TRUE if valid, FALSE otherwise
validate_input <- function(data, rules, error_message = NULL, 
                          log_error = TRUE, notify_user = FALSE, session = NULL) {
  for (rule in rules) {
    if (!rule$check(data)) {
      msg <- if (!is.null(error_message)) {
        paste0(error_message, ": ", rule$message)
      } else {
        rule$message
      }
      
      # Log error
      if (log_error) {
        log_error(msg)
      }
      
      # Notify user
      if (notify_user && !is.null(session)) {
        session$sendCustomMessage("notification", 
                                 list(type = "error", 
                                      message = paste("Validation Error:", msg)))
      }
      
      return(FALSE)
    }
  }
  
  return(TRUE)
}

#' Create a validation rule
#' 
#' This function creates a validation rule
#' 
#' @param check_fn The function to check the data
#' @param message The error message
#' @return A validation rule
validation_rule <- function(check_fn, message) {
  list(
    check = check_fn,
    message = message
  )
}

# Common validation rules
is_not_null <- function(message = "Value cannot be NULL") {
  validation_rule(function(x) !is.null(x), message)
}

is_not_na <- function(message = "Value cannot be NA") {
  validation_rule(function(x) !any(is.na(x)), message)
}

is_numeric <- function(message = "Value must be numeric") {
  validation_rule(function(x) is.numeric(x), message)
}

is_positive <- function(message = "Value must be positive") {
  validation_rule(function(x) is.numeric(x) && all(x > 0), message)
}

is_non_negative <- function(message = "Value must be non-negative") {
  validation_rule(function(x) is.numeric(x) && all(x >= 0), message)
}

is_in_range <- function(min, max, message = NULL) {
  if (is.null(message)) {
    message <- paste("Value must be between", min, "and", max)
  }
  validation_rule(function(x) is.numeric(x) && all(x >= min) && all(x <= max), message)
}

is_character <- function(message = "Value must be a character string") {
  validation_rule(function(x) is.character(x), message)
}

has_length <- function(min_length, message = NULL) {
  if (is.null(message)) {
    message <- paste("Value must have at least", min_length, "characters")
  }
  validation_rule(function(x) is.character(x) && all(nchar(x) >= min_length), message)
}

is_valid_date <- function(message = "Value must be a valid date") {
  validation_rule(function(x) !is.na(as.Date(x, optional = TRUE)), message)
}

#' Safely update player selection dropdown
#' 
#' This function safely updates the player selection dropdown with proper error handling
#' to prevent the "argument is of length zero" error.
#' 
#' @param session Shiny session object
#' @param input_id ID of the select input to update
#' @param player_data Function that returns player data
#' @param player_type_input Input value for player type (batter or pitcher)
#' @return Nothing, updates UI element
update_player_selection_safely <- function(session, input_id, player_data, player_type_input) {
  # Use safe_call for proper error handling
  safe_call({
    # Check if player_data is a function
    if (!is.function(player_data)) {
      log_warning("player_data is not a function in advanced metrics module")
      updateSelectInput(session, input_id, 
                       choices = setNames(c(""), c("No players available")),
                       selected = "")
      return(invisible(NULL))
    }
    
    # Try to get players data with defensive programming
    players <- tryCatch({
      if (is.function(player_data)) {
        player_data()
      } else {
        NULL
      }
    }, error = function(e) {
      log_error(paste("Error getting player data:", e$message))
      return(NULL)
    })
    
    # Check if players data exists and has required columns
    if (is.null(players) || nrow(players) == 0) {
      log_warning("Empty player data in advanced metrics module")
      updateSelectInput(session, input_id, 
                       choices = setNames(c(""), c("No players available")),
                       selected = "")
      return(invisible(NULL))
    }
    
    # Check for required columns
    required_cols <- c("player_id", "player_name", "is_pitcher")
    if (!all(required_cols %in% names(players))) {
      log_warning(paste("Missing required columns in player data:", 
                        paste(setdiff(required_cols, names(players)), collapse=", ")))
      updateSelectInput(session, input_id, 
                       choices = setNames(c(""), c("Invalid player data")),
                       selected = "")
      return(invisible(NULL))
    }
    
    # Filter by player type with defensive programming
    if (!is.null(player_type_input)) {
      if (player_type_input == "batter") {
        filtered_players <- players[!is.na(players$is_pitcher) & players$is_pitcher == FALSE, ]
      } else {
        filtered_players <- players[!is.na(players$is_pitcher) & players$is_pitcher == TRUE, ]
      }
    } else {
      # Default to all players if player_type is not set
      filtered_players <- players
      log_info("No player_type selected, showing all players")
    }
    
    # Check if we have any players after filtering
    if (is.null(filtered_players) || nrow(filtered_players) == 0) {
      player_type_msg <- if (!is.null(player_type_input)) player_type_input else "player"
      no_players_msg <- paste0("No ", player_type_msg, "s available")
      log_warning(no_players_msg)
      updateSelectInput(session, input_id, 
                       choices = setNames(c(""), c(no_players_msg)),
                       selected = "")
      return(invisible(NULL))
    }
    
    # Create choices for select input with defensive programming
    player_ids <- as.character(filtered_players$player_id)
    player_names <- as.character(filtered_players$player_name)
    
    # Ensure we have valid names and IDs
    valid_indices <- !is.na(player_ids) & !is.na(player_names) & player_ids != "" & player_names != ""
    if (sum(valid_indices) == 0) {
      log_warning("No valid player IDs or names found after filtering")
      updateSelectInput(session, input_id, 
                       choices = setNames(c(""), c("No valid players")),
                       selected = "")
      return(invisible(NULL))
    }
    
    # Use only valid entries
    choices <- setNames(player_ids[valid_indices], player_names[valid_indices])
    
    # Update select input
    updateSelectInput(session, input_id, choices = choices, 
                     selected = if(length(choices) > 0) choices[1] else "")
    log_info(paste("Updated player selection with", length(choices), "players"))
    
    return(invisible(NULL))
  }, 
  error_message = "Error in advanced metrics player selection", 
  default_value = NULL,
  log_error = TRUE)
}
