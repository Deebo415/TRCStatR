# League Averages Calculation Module
# Uses object_engine.R for raw data and calculations

# Source centralized data and calculations
source("R/object_engine.R")

# Declare global variables to avoid lint warnings
utils::globalVariables(c(
  # Data frames
  "batting_stats", "pitching_stats", "League_BA", "League_OBP", "League_SLG", "League_OPS",
  "League_ISO", "League_BB_Percent", "League_SB_Percent", "League_BABIP", "League_ERA", 
  "League_WHIP", "League_K9", "League_BB9", "League_HR9", "FIP_constant", "League_FIP", "League_KBB",
  
  # Variables
  "TB", "plate_app", "H", "home_runs", "walks", "hit_by_pitch", "strikeouts", "sac_flies",
  "AB", "singles", "doubles", "triples", "batters_faced", "batters_struck_out", "walks_allowed",
  "hr_allowed", "batters_hit", "IP", "ER", "earned_runs", "outs_recorded", "steals",
  
  # Functions
  "safe_divide", "safe_batting_avg", "safe_obp", "safe_slg", "safe_era", "safe_whip", "safe_per_nine"
))

#' Calculate league batting averages with validation
#'
#' Calculates all batting average metrics with robust validation
#' @param batting_data Data frame containing batting statistics
#' @param verbose Whether to print detailed diagnostic information
#' @return Data frame containing league batting averages
calculate_league_batting_averages <- function(batting_data, verbose = FALSE) {
  # Default values for youth baseball
  default_values <- data.frame(
    League_AVG = 0.250,
    League_OBP = 0.320,
    League_SLG = 0.400,
    League_OPS = 0.720,
    League_ISO = 0.150,
    League_TB_per_PA = 0.500,
    League_BB_per_PA = 0.100,
    League_SB_per_PA = 0.050,
    League_BABIP = 0.300,
    League_PA_per_Game = 4.500,
    League_RPerWin = 10.000
  )
  
  # Ensure we have data with robust error handling
  tryCatch({
    if (is.null(batting_data) || nrow(batting_data) == 0) {

      return(default_values)
    }
  }, error = function(e) {

    return(default_values)
  })
  
  if (verbose) {

  }
  
  # Leverage the safe_* functions from object_engine.R
  # Use directly calculated values from object_engine if they exist
  # Otherwise calculate them with safe_* functions
  
  # Extract key statistics with error handling
  tryCatch({
    # Use the field names from object_engine.R (X1B, X2B, X3B)
    singles <- sum(batting_data$X1B, na.rm = TRUE)
    doubles <- sum(batting_data$X2B, na.rm = TRUE)
    triples <- sum(batting_data$X3B, na.rm = TRUE)
    home_runs <- sum(batting_data$HR, na.rm = TRUE)
    walks <- sum(batting_data$BB, na.rm = TRUE)
    hit_by_pitch <- sum(batting_data$HBP, na.rm = TRUE)
    plate_app <- sum(batting_data$PA, na.rm = TRUE)
    sac_flies <- sum(batting_data$SF, na.rm = TRUE)
    sac_hits <- sum(batting_data$SH, na.rm = TRUE)
    strikeouts <- sum(batting_data$K, na.rm = TRUE)
    steals <- sum(batting_data$SB, na.rm = TRUE)
    
    # Calculate derived values
    AB <- plate_app - walks - hit_by_pitch - sac_flies - sac_hits
    H <- singles + doubles + triples + home_runs
    TB <- singles + (2 * doubles) + (3 * triples) + (4 * home_runs)
    
    # Calculate league averages using safe functions
    league_avg <- safe_batting_avg(H, AB, default = 0.250)
    league_obp <- safe_obp(H, walks, hit_by_pitch, AB, sac_flies, default = 0.320)
    league_slg <- safe_slg(TB, AB, default = 0.400)
    league_ops <- league_obp + league_slg
  }, error = function(e) {

    league_avg <- 0.250
    league_obp <- 0.320
    league_slg <- 0.400
    league_ops <- 0.720
  })
  # Calculate additional metrics with error handling
  tryCatch({
    league_babip <- safe_divide(H - home_runs, AB - strikeouts - home_runs + sac_flies, default = 0.300)
    league_iso <- league_slg - league_avg
    league_bb_pct <- safe_divide(walks, plate_app, default = 0.100)
    league_sb_pct <- safe_divide(steals, plate_app, default = 0.050)
    league_tb_per_pa <- safe_divide(TB, plate_app, default = 0.500)
  }, error = function(e) {

    league_babip <- 0.300
    league_iso <- 0.150
    league_bb_pct <- 0.100
    league_sb_pct <- 0.050
    league_tb_per_pa <- 0.500
  })
  
  if (verbose) {

  }
  
  # Return as a data frame
  return(data.frame(
    League_AVG = league_avg,
    League_OBP = league_obp,
    League_SLG = league_slg,
    League_OPS = league_ops,
    League_ISO = league_iso,
    League_TB_per_PA = league_tb_per_pa,
    League_BB_per_PA = league_bb_pct,
    League_SB_per_PA = league_sb_pct,
    League_BABIP = league_babip,
    League_PA_per_Game = 4.5,  # This is typically a constant
    League_RPerWin = 10.0      # This is typically a constant
  ))
}

#' Calculate league pitching averages with validation
#'
#' Calculates all pitching average metrics with robust validation
#' @param pitching_data Data frame containing pitching statistics
#' @param verbose Whether to print detailed diagnostic information
#' @return Data frame containing league pitching averages
calculate_league_pitching_averages <- function(pitching_data, verbose = FALSE) {
  # Default values for youth baseball
  default_values <- data.frame(
    League_ERA = 4.50,
    League_WHIP = 1.30,
    League_K_per_IP = 1.00,
    League_BB_per_IP = 0.50,
    League_HR_per_IP = 0.10,
    League_BF_per_IP = 4.30,
    League_FIP = 4.50,
    League_FIP_Constant = 3.10,
    League_KBB = 2.00
  )
  
  # Ensure we have data with robust error handling
  tryCatch({
    if (is.null(pitching_data) || nrow(pitching_data) == 0) {

      return(default_values)
    }
  }, error = function(e) {

    return(default_values)
  })
  
  if (verbose) {

  }
  
  # Leverage safe functions from object_engine.R
  # Calculate league averages with error handling
  tryCatch({
    league_era <- safe_era(earned_runs, IP, default = 4.50)
    league_whip <- safe_whip(walks_allowed, hits_allowed, IP, batters_hit, default = 1.30)
    league_k_per_ip <- safe_per_nine(batters_struck_out, IP, default = 1.00) / 9
    league_bb_per_ip <- safe_per_nine(walks_allowed, IP, default = 0.50) / 9
    league_hr_per_ip <- safe_per_nine(hr_allowed, IP, default = 0.10) / 9
    league_bf_per_ip <- safe_divide(batters_faced, IP, default = 4.30)
  }, error = function(e) {

    league_era <- 4.50
    league_whip <- 1.30
    league_k_per_ip <- 1.00
    league_bb_per_ip <- 0.50
    league_hr_per_ip <- 0.10
    league_bf_per_ip <- 4.30
  })
  # Calculate additional metrics with error handling
  tryCatch({
    league_kbb <- safe_divide(batters_struck_out, walks_allowed, default = 2.00)
    
    # Calculate FIP components
    league_fip_numerator <- (13 * hr_allowed) + 
      (3 * (walks_allowed + batters_hit)) - 
      (2 * batters_struck_out)
    league_fip_raw <- safe_divide(league_fip_numerator, IP, default = 4.50)
    
    # FIP constant to align with ERA
    league_fip_constant <- league_era - league_fip_raw
    league_fip <- league_fip_raw + league_fip_constant
  }, error = function(e) {

    league_kbb <- 2.00
    league_fip_constant <- 3.10
    league_fip <- 4.50
  })
  
  if (verbose) {

  }
  
  # Return as a data frame
  return(data.frame(
    League_ERA = league_era,
    League_WHIP = league_whip,
    League_K_per_IP = league_k_per_ip,
    League_BB_per_IP = league_bb_per_ip,
    League_HR_per_IP = league_hr_per_ip,
    League_BF_per_IP = league_bf_per_ip,
    League_FIP = league_fip,
    League_FIP_Constant = league_fip_constant,
    League_KBB = league_kbb
  ))
}

#' Calculate all league averages
#'
#' Calculates both batting and pitching league averages
#' @param verbose Whether to print detailed diagnostic information
#' @return List containing batting and pitching league averages
calculate_all_league_averages <- function(verbose = FALSE) {
  # Leverage data directly from object_engine.R
  # Calculate batting averages using the full dataset
  batting_averages <- calculate_league_batting_averages(batting_stats, verbose)
  
  # Calculate pitching averages
  pitching_averages <- calculate_league_pitching_averages(pitching_stats, verbose)
  
  # Return both sets of averages
  return(list(
    batting = batting_averages,
    pitching = pitching_averages
  ))
}

#' Get league averages
#'
#' Retrieves league averages or calculates them if not available
#' @param division_id Division ID
#' @param season_year Season year
#' @param force_recalculate Whether to force recalculation even if values exist
#' @param verbose Whether to print detailed diagnostic information
#' @return List containing batting and pitching league averages
get_league_averages <- function(division_id = 1, season_year = 2023, force_recalculate = FALSE, verbose = FALSE) {
  # First look for values already calculated in object_engine.R with error handling
  tryCatch({
    if (!force_recalculate) {
      # Check if core variables from object_engine.R are available
      if (exists("League_BA") && exists("League_OBP") && exists("League_SLG") && 
          exists("League_OPS") && exists("League_ERA") && exists("League_WHIP")) {
      
      # Values exist, construct return structure from existing variables
      batting_avgs <- data.frame(
        League_AVG = League_BA,
        League_OBP = League_OBP,
        League_SLG = League_SLG,
        League_OPS = League_OPS,
        League_ISO = League_ISO,
        League_TB_per_PA = safe_divide(sum(TB, na.rm = TRUE), sum(plate_app, na.rm = TRUE), default = 0.500),
        League_BB_per_PA = League_BB_Percent,
        League_SB_per_PA = League_SB_Percent,
        League_BABIP = League_BABIP,
        League_PA_per_Game = 4.5,
        League_RPerWin = 10.0
      )
      
      pitching_avgs <- data.frame(
        League_ERA = League_ERA,
        League_WHIP = League_WHIP,
        League_K_per_IP = League_K9 / 9,
        League_BB_per_IP = League_BB9 / 9,
        League_HR_per_IP = League_HR9 / 9,
        League_BF_per_IP = 4.3,  # Approximation
        League_FIP = League_FIP,
        League_FIP_Constant = FIP_constant,
        League_KBB = League_KBB
      )
      

      
      return(list(
        batting = batting_avgs,
        pitching = pitching_avgs
      ))
    }
  }
  
  # Calculate and return

  return(calculate_all_league_averages(verbose))
  }, error = function(e) {

    return(calculate_all_league_averages(verbose))
  })
}