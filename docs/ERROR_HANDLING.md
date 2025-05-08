# Error Handling System Documentation

## Overview

The TRCStatR application implements a comprehensive error handling system designed to ensure application stability, provide meaningful feedback to users, and facilitate debugging. This document explains the error handling approach, key components, and best practices for using the system.

## Key Components

### 1. Safe Function Execution

The core of the error handling system is the `safe_call()` function, which provides a consistent way to execute functions with proper error handling.

```r
safe_call <- function(expr, error_message = NULL, default_value = NULL, 
                     log_error = FALSE, notify_user = FALSE) {
  tryCatch(
    {
      # Attempt to evaluate the expression
      result <- expr
      return(result)
    },
    error = function(e) {
      # Handle any errors that occur
      if (log_error) {
        log_error(paste0(error_message, ": ", e$message))
      }
      
      if (notify_user && !is.null(error_message)) {
        show_error_notification(error_message)
      }
      
      return(default_value)
    },
    warning = function(w) {
      # Handle any warnings
      log_warning(paste0("Warning in expression: ", w$message))
      
      # Attempt to evaluate the expression despite the warning
      result <- expr
      return(result)
    }
  )
}
```

### 2. Database-Specific Error Handling

For database operations, the `safe_db_call()` function extends `safe_call()` with database-specific error handling.

```r
safe_db_call <- function(db_func, error_message = NULL, default_value = NULL, 
                        log_error = TRUE, notify_user = TRUE) {
  safe_call(
    {
      # Get a connection from the pool
      con <- get_connection_pool()
      
      # Execute the database function with the connection
      result <- db_func(con)
      
      # Return the result
      return(result)
    },
    error_message = error_message,
    default_value = default_value,
    log_error = log_error,
    notify_user = notify_user
  )
}
```

### 3. Logging System

The logging system provides different levels of logging (ERROR, WARNING, INFO, DEBUG) to capture relevant information for troubleshooting.

```r
log_error <- function(message) {
  if (get_log_level() <= LOG_LEVEL_ERROR) {
    log_message("ERROR", message)
  }
}

log_warning <- function(message) {
  if (get_log_level() <= LOG_LEVEL_WARNING) {
    log_message("WARNING", message)
  }
}

log_info <- function(message) {
  if (get_log_level() <= LOG_LEVEL_INFO) {
    log_message("INFO", message)
  }
}

log_debug <- function(message) {
  if (get_log_level() <= LOG_LEVEL_DEBUG) {
    log_message("DEBUG", message)
  }
}
```

### 4. User Notifications

The system can display user-friendly error messages to inform users when issues occur.

```r
show_error_notification <- function(message) {
  if (exists("session") && !is.null(session)) {
    session$sendCustomMessage("notification", list(
      type = "error",
      message = message
    ))
  }
}

show_success_notification <- function(message) {
  if (exists("session") && !is.null(session)) {
    session$sendCustomMessage("notification", list(
      type = "success",
      message = message
    ))
  }
}
```

### 5. Input Validation

The input validation system prevents errors by ensuring data meets expected criteria before processing.

```r
validate_player_data <- function(player_data) {
  # Check required fields
  required_fields <- c("name", "team_id", "position")
  for (field in required_fields) {
    if (is.null(player_data[[field]]) || player_data[[field]] == "") {
      return(list(valid = FALSE, message = paste0("Missing required field: ", field)))
    }
  }
  
  # Validate specific fields
  if (!is.numeric(player_data$jersey_number) || player_data$jersey_number < 0) {
    return(list(valid = FALSE, message = "Jersey number must be a positive number"))
  }
  
  # All validations passed
  return(list(valid = TRUE, message = NULL))
}
```

## Error Handling Workflow

The typical error handling workflow in TRCStatR follows these steps:

1. **Validate Input**: Check that all inputs meet the required criteria
2. **Execute with Safe Call**: Use `safe_call()` or `safe_db_call()` to execute functions
3. **Handle Results**: Check the result and provide appropriate feedback
4. **Log Information**: Log relevant information for debugging and monitoring

Example:

```r
add_new_player <- function(player_data) {
  # Step 1: Validate input
  validation <- validate_player_data(player_data)
  if (!validation$valid) {
    log_error(paste0("Invalid player data: ", validation$message))
    show_error_notification(validation$message)
    return(FALSE)
  }
  
  # Step 2: Execute with safe call
  result <- safe_db_call(
    function(con) {
      query <- "INSERT INTO players (name, team_id, position, jersey_number) VALUES (?, ?, ?, ?)"
      params <- list(player_data$name, player_data$team_id, player_data$position, player_data$jersey_number)
      execute_query(con, query, params)
    },
    error_message = "Failed to add new player",
    default_value = FALSE,
    log_error = TRUE,
    notify_user = TRUE
  )
  
  # Step 3: Handle results
  if (result) {
    log_info(paste0("Added new player: ", player_data$name))
    show_success_notification(paste0("Successfully added player: ", player_data$name))
  }
  
  return(result)
}
```

## Best Practices

### 1. Always Use Safe Call Functions

Wrap all error-prone operations in `safe_call()` or `safe_db_call()` to ensure consistent error handling.

```r
# Good
result <- safe_call(
  complex_calculation(data),
  error_message = "Error in calculation",
  default_value = NA
)

# Avoid
result <- complex_calculation(data)  # No error handling
```

### 2. Provide Meaningful Error Messages

Error messages should be clear, specific, and actionable for both users and developers.

```r
# Good
error_message = "Failed to load player data for team ID 5"

# Avoid
error_message = "Error loading data"  # Too vague
```

### 3. Set Appropriate Default Values

Choose default values that allow the application to continue functioning when possible.

```r
# Good - Empty data frame with correct structure
default_value = data.frame(player_id = integer(0), name = character(0), team_id = integer(0))

# Avoid
default_value = NULL  # May cause downstream errors
```

### 4. Log Sufficient Context

Include relevant context in log messages to facilitate debugging.

```r
# Good
log_error(paste0("Failed to update player ID ", player_id, ": ", e$message))

# Avoid
log_error("Update failed")  # Insufficient context
```

### 5. Validate Early, Validate Often

Validate inputs as early as possible to prevent errors from occurring.

```r
# Good - Validate at function entry
add_game_stats <- function(game_id, player_id, stats) {
  if (is.null(game_id) || !is.numeric(game_id)) {
    return(list(success = FALSE, message = "Invalid game ID"))
  }
  # Continue with function...
}

# Avoid - Late validation
add_game_stats <- function(game_id, player_id, stats) {
  # Lots of processing...
  # Only validate before database call
  if (is.null(game_id) || !is.numeric(game_id)) {
    return(list(success = FALSE, message = "Invalid game ID"))
  }
}
```

### 6. Use Appropriate Log Levels

Choose the appropriate log level based on the severity and importance of the message.

```r
# Critical errors
log_error("Database connection failed")

# Potential issues
log_warning("Using default values for missing configuration")

# Normal operation events
log_info("User logged in: user123")

# Detailed debugging information
log_debug("Query execution time: 0.05s")
```

## Common Error Scenarios and Solutions

### Database Connection Issues

```r
# Problem: Database connection fails
db_connection <- safe_call(
  dbConnect(RSQLite::SQLite(), db_path),
  error_message = "Failed to connect to database",
  default_value = NULL,
  log_error = TRUE
)

# Solution: Check if connection exists before using
if (is.null(db_connection)) {
  # Use fallback data or show appropriate message
  show_error_notification("Database unavailable. Some features may be limited.")
}
```

### Division by Zero

```r
# Problem: Division by zero in calculations
batting_avg <- safe_call(
  hits / at_bats,
  error_message = "Error calculating batting average",
  default_value = 0,
  log_error = TRUE
)

# Better solution: Use safe_divide function
batting_avg <- safe_divide(hits, at_bats, default = 0)
```

### Missing Data

```r
# Problem: Accessing missing data
player_name <- safe_call(
  player_data$name,
  error_message = "Player name not found",
  default_value = "Unknown Player",
  log_error = TRUE
)

# Better solution: Check existence before accessing
player_name <- if (is.null(player_data) || is.null(player_data$name)) {
  "Unknown Player"
} else {
  player_data$name
}
```

## Conclusion

The TRCStatR error handling system provides a robust framework for managing errors, ensuring application stability, and providing a good user experience. By following the best practices outlined in this document, developers can maintain and extend the application with confidence.

For more detailed information on specific error handling functions, refer to the source code in `error_handling.R`.
