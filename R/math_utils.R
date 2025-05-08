# math_utils.R
# Module for safe mathematical operations in TRCStatR

#' Safely divide two numbers, handling division by zero
#'
#' This function performs division with proper handling of division by zero,
#' NA values, and other edge cases. It returns a default value when division
#' is not possible.
#'
#' @param numerator The numerator in the division operation
#' @param denominator The denominator in the division operation
#' @param default The default value to return if division is not possible (default: 0)
#' @param min_denominator The minimum value for the denominator to prevent near-zero division (default: 1e-10)
#' @return The result of the division or the default value
safe_divide <- function(numerator, denominator, default = 0, min_denominator = 1e-10) {
  # Handle NULLs up front
  if (is.null(numerator) || is.null(denominator)) {
    return(default)
  }
  # Recycle scalars to match vector lengths
  numerator <- as.numeric(numerator)
  denominator <- as.numeric(denominator)
  len <- max(length(numerator), length(denominator))
  if (length(numerator) != len) numerator <- rep(numerator, len)
  if (length(denominator) != len) denominator <- rep(denominator, len)
  # Vectorized safe division (fix: only use | not ||)
  # If denominator is NA or near zero, return default for that element
  invalid <- is.na(numerator) | is.na(denominator) | abs(denominator) < min_denominator
  result <- rep(default, len)
  # Only divide where valid
  result[!invalid] <- numerator[!invalid] / denominator[!invalid]
  return(result)
}

#' Safely calculate a batting average
#'
#' This function calculates a batting average (hits / at-bats) with proper
#' handling of edge cases.
#'
#' @param hits Number of hits
#' @param at_bats Number of at-bats
#' @param default Default value to return if calculation is not possible (default: 0)
#' @return The calculated batting average or the default value
safe_batting_avg <- function(hits, at_bats, default = 0) {
  avg <- safe_divide(hits, at_bats, default)
  # Ensure the result is between 0 and 1
  return(max(0, min(1, avg)))
}

#' Safely calculate on-base percentage
#'
#' This function calculates on-base percentage ((hits + walks + hbp) / (at-bats + walks + hbp + sf))
#' with proper handling of edge cases.
#'
#' @param hits Number of hits
#' @param walks Number of walks
#' @param hbp Number of hit by pitch
#' @param at_bats Number of at-bats
#' @param sf Number of sacrifice flies
#' @param default Default value to return if calculation is not possible (default: 0)
#' @return The calculated on-base percentage or the default value
safe_obp <- function(hits, walks, hbp, at_bats, sf, default = 0) {
  obp <- safe_divide(hits + walks + hbp, at_bats + walks + hbp + sf, default)
  # Ensure the result is between 0 and 1
  return(max(0, min(1, obp)))
}

#' Safely calculate slugging percentage
#'
#' This function calculates slugging percentage (total bases / at-bats) with proper
#' handling of edge cases.
#'
#' @param total_bases Total bases (singles + 2*doubles + 3*triples + 4*home_runs)
#' @param at_bats Number of at-bats
#' @param default Default value to return if calculation is not possible (default: 0)
#' @return The calculated slugging percentage or the default value
safe_slg <- function(total_bases, at_bats, default = 0) {
  slg <- safe_divide(total_bases, at_bats, default)
  # Ensure the result is reasonable (technically SLG can be > 1)
  return(max(0, slg))
}

#' Safely calculate ERA (Earned Run Average)
#'
#' This function calculates ERA (9 * earned runs / innings pitched) with proper
#' handling of edge cases.
#'
#' @param earned_runs Number of earned runs
#' @param innings_pitched Number of innings pitched
#' @param default Default value to return if calculation is not possible (default: 9.99)
#' @param max_era Maximum ERA to return (default: 99.99)
#' @return The calculated ERA or the default value
safe_era <- function(earned_runs, innings_pitched, default = 9.99, max_era = 99.99) {
  era <- safe_divide(9 * earned_runs, innings_pitched, default)
  # Cap extremely high ERAs
  return(min(era, max_era))
}

#' Safely calculate WHIP (Walks, Hits, and Hit Batsmen per Inning Pitched)
#'
#' This function calculates WHIP ((walks + hits + hit batsmen) / innings pitched) with proper
#' handling of edge cases. For youth baseball, we include hit batsmen in the WHIP calculation
#' since they are more common at this level.
#'
#' @param walks Number of walks
#' @param hits Number of hits
#' @param innings_pitched Number of innings pitched
#' @param hit_batsmen Number of hit batsmen (default: 0)
#' @param default Default value to return if calculation is not possible (default: 2.00)
#' @param max_whip Maximum WHIP to return (default: 9.99)
#' @return The calculated WHIP or the default value
safe_whip <- function(walks, hits, innings_pitched, hit_batsmen = 0, default = 2.00, max_whip = 9.99) {
  whip <- safe_divide(walks + hits + hit_batsmen, innings_pitched, default)
  # Cap extremely high WHIPs
  return(min(whip, max_whip))
}

#' Safely calculate a rate statistic per 9 innings
#'
#' This function calculates a rate statistic per 9 innings (9 * value / innings pitched)
#' with proper handling of edge cases.
#'
#' @param value The value to calculate the rate for (e.g., strikeouts, walks)
#' @param innings_pitched Number of innings pitched
#' @param default Default value to return if calculation is not possible (default: 0)
#' @return The calculated rate per 9 innings or the default value
safe_per_nine <- function(value, innings_pitched, default = 0) {
  return(safe_divide(9 * value, innings_pitched, default))
}

#' Safely calculate OPS+ (On-base Plus Slugging Plus)
#'
#' This function calculates OPS+ (100 * player_ops / league_ops) with proper
#' handling of edge cases.
#'
#' @param player_ops Player's OPS (on-base percentage + slugging percentage)
#' @param league_ops League average OPS
#' @param default Default value to return if calculation is not possible (default: 100)
#' @return The calculated OPS+ or the default value
safe_ops_plus <- function(player_ops, league_ops, default = 100) {
  return(round(safe_divide(100 * player_ops, league_ops, default)))
}

#' Safely calculate ERA+ (ERA Plus)
#'
#' This function calculates ERA+ (100 * league_era / player_era) with proper
#' handling of edge cases.
#'
#' @param player_era Player's ERA
#' @param league_era League average ERA
#' @param default Default value to return if calculation is not possible (default: 100)
#' @return The calculated ERA+ or the default value
safe_era_plus <- function(player_era, league_era, default = 100) {
  # Add a small constant (0.01) to player_era to account for dominant pitchers with 0.00 ERA
  # This ensures that truly dominant pitchers still get very high ERA+ values
  player_era <- player_era + 0.01
  
  # Ensure player_era is not zero or negative (though this should be impossible now)
  if (player_era <= 0) {
    return(default)
  }
  
  return(round(safe_divide(100 * league_era, player_era, default)))
}

#' Safely calculate WHIP+ (WHIP Plus)
#'
#' This function calculates WHIP+ (100 * league_whip / player_whip) with proper
#' handling of edge cases, including adding a small constant for dominant pitchers.
#'
#' @param player_whip Player's WHIP
#' @param league_whip League average WHIP
#' @param default Default value to return if calculation is not possible (default: 100)
#' @return The calculated WHIP+ or the default value
safe_whip_plus <- function(player_whip, league_whip, default = 100) {
  # Add a small constant (0.01) to player_whip to account for dominant pitchers with 0.000 WHIP
  # This ensures that truly dominant pitchers still get very high WHIP+ values
  player_whip <- player_whip + 0.01
  
  # Ensure player_whip is not zero or negative (though this should be impossible now)
  if (player_whip <= 0) {
    return(default)
  }
  
  return(round(safe_divide(100 * league_whip, player_whip, default)))
}

#' Safely calculate a scaling factor
#'
#' This function calculates a scaling factor (total / parts) with proper
#' handling of edge cases.
#'
#' @param total The total value (e.g., season length)
#' @param parts The parts value (e.g., games played)
#' @param default Default value to return if calculation is not possible (default: 1)
#' @return The calculated scaling factor or the default value
safe_scale_factor <- function(total, parts, default = 1) {
  return(safe_divide(total, parts, default))
}
