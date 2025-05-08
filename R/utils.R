# utils.R - General utility functions for TRCStatR

# --- Defensive helpers for UI and data handling ---

# Use object_engine's get_player_names() function if it exists, otherwise define it here
if (!exists("get_player_names")) {
  # --- Utility: Get player names from IDs ---
  get_player_names <- function(ids) {
    if (is.null(ids) || length(ids) == 0) return(character(0))
    name_lookup <- setNames(paste(players$FirstName, players$LastInitial), as.character(players$PlayerID))
    name_lookup[as.character(ids)]
  }
}

# --- Defensive setNames for UI labels (IDs as values, names as labels) ---
safe_setNames <- function(ids) {
  if (is.null(ids) || length(ids) == 0) return(setNames(character(0), character(0)))
  nms <- get_player_names(ids)
  setNames(ids, nms)
}

# --- Defensive numerical operations ---
# safe_divide is now only defined in math_utils.R for robust vectorized behavior.

# --- Null-safe operators ---
`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

# --- Data validation helpers ---
is_valid_id <- function(id) {
  if (is.null(id) || is.na(id)) return(FALSE)
  id_numeric <- suppressWarnings(as.numeric(as.character(id)))
  return(!is.na(id_numeric) && id_numeric > 0)
}

# --- Data transformation helpers ---
convert_to_numeric <- function(df, numeric_columns) {
  for (col in numeric_columns) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  return(df)
}

# --- Debug helpers ---
print_debug <- function(msg, level = "INFO") {
  if (exists("DEBUG_LEVEL") && DEBUG_LEVEL == TRUE) {
    cat(paste0("[", level, "] ", format(Sys.time(), "%H:%M:%S"), " - ", msg, "\n"))
  }
}