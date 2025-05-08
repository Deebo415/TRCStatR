# Helper functions for normalizing and plotting player metric data

normalize_metrics <- function(df, metric_cols) {
  # Z-score normalization for each metric column (per column)
  for (col in metric_cols) {
    vals <- as.numeric(df[[col]])
    if (length(unique(vals)) > 1) {
      df[[col]] <- (vals - mean(vals, na.rm=TRUE)) / sd(vals, na.rm=TRUE)
    } else {
      df[[col]] <- 0  # If all values are the same, set to 0
    }
  }
  df
}

minmax_metrics <- function(df, metric_cols) {
  # Min-max normalization for each metric column (per column)
  for (col in metric_cols) {
    vals <- as.numeric(df[[col]])
    rng <- range(vals, na.rm=TRUE)
    if (diff(rng) > 0) {
      df[[col]] <- (vals - rng[1]) / diff(rng)
    } else {
      df[[col]] <- 0.5  # If all values are the same, set to mid
    }
  }
  df
}
