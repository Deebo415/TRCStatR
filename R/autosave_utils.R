# autosave_utils.R - Utilities for periodic auto-save of game entry progress

#' Save the current game entry form state to a temporary RDS file
#' @param form_state A list containing all relevant form state (batters, pitchers, input values)
#' @param path Path to the autosave RDS file
save_autosave_state <- function(form_state, path = "game_entry_autosave.rds") {
  tryCatch({
    saveRDS(form_state, path)
    TRUE
  }, error = function(e) {
    warning(sprintf("Error auto-saving game entry: %s", e$message))
    FALSE
  })
}

#' Load the autosave state from the temporary RDS file
#' @param path Path to the autosave RDS file
#' @return The form state list, or NULL if not found or error
load_autosave_state <- function(path = "game_entry_autosave.rds") {
  if (!file.exists(path)) return(NULL)
  tryCatch({
    readRDS(path)
  }, error = function(e) {
    warning(sprintf("Error loading autosave state: %s", e$message))
    NULL
  })
}

#' Clear the autosave file after successful game entry
#' @param path Path to the autosave RDS file
clear_autosave_state <- function(path = "game_entry_autosave.rds") {
  if (file.exists(path)) {
    tryCatch({
      file.remove(path)
      TRUE
    }, error = function(e) {
      warning(sprintf("Error clearing autosave state: %s", e$message))
      FALSE
    })
  } else {
    TRUE
  }
}
