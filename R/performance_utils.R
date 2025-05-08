# performance_utils.R
# Module for optimized performance in statistical calculations

library(data.table)
library(bit64)
library(future)
library(future.apply)

# Set up parallel processing if available
if (requireNamespace("future", quietly = TRUE)) {
  # Use multisession instead of multiprocess for better compatibility
  plan(future::multisession, workers = min(parallel::detectCores() - 1, 2))
} else {
  # If future package is not available, use sequential processing
  warning("The 'future' package is not available. Using sequential processing instead.")
  if (exists("plan")) plan(future::sequential)
}

#' Vectorized safe division
#'
#' This function performs vectorized division with proper handling of division by zero,
#' NA values, and other edge cases. It's optimized for large datasets.
#'
#' @param numerator Vector of numerators
#' @param denominator Vector of denominators
#' @param default Default value to return if division is not possible
#' @param min_denominator Minimum value for denominator to prevent near-zero division
#' @return Vector of division results
vectorized_safe_divide <- function(numerator, denominator, default = 0, min_denominator = 1e-10) {
  # Add comprehensive input validation
  tryCatch({
    # Check for NULL inputs
    if (is.null(numerator) || is.null(denominator)) {

      return(default)
    }
    
    # Check for empty inputs
    if (length(numerator) == 0 || length(denominator) == 0) {

      return(default)
    }
    
    # Check for non-numeric inputs
    if (!is.numeric(numerator) || !is.numeric(denominator)) {

      return(default)
    }
    
    # Handle different length inputs
    if (length(numerator) != length(denominator)) {
      if (length(numerator) == 1) {
        # Scalar numerator with vector denominator
        numerator <- rep(numerator, length(denominator))
      } else if (length(denominator) == 1) {
        # Vector numerator with scalar denominator
        denominator <- rep(denominator, length(numerator))
      } else {
        # Incompatible lengths

        return(default)
      }
    }
    
    # Pre-allocate result vector for better performance
    result <- rep(default, length(numerator))
    
    # Create a logical mask for valid divisions
    valid_indices <- !is.na(numerator) & !is.na(denominator) & abs(denominator) >= min_denominator
    
    # Check if valid_indices is empty or all FALSE
    if (length(valid_indices) == 0 || !any(valid_indices)) {

      return(default)
    }
    
    # Only perform division on valid indices
    result[valid_indices] <- numerator[valid_indices] / denominator[valid_indices]
    
    return(result)
  }, error = function(e) {

    return(default)
  })
}

#' Batch process statistical calculations
#'
#' This function processes statistical calculations in batches for better performance
#' with large datasets.
#'
#' @param data Data frame containing the raw data
#' @param batch_size Size of each batch for processing
#' @param calculation_fn Function to apply to each batch
#' @return Combined results from all batches
batch_process <- function(data, batch_size = 1000, calculation_fn) {
  # Convert to data.table for faster processing
  dt <- as.data.table(data)
  
  # Calculate number of batches
  n_rows <- nrow(dt)
  n_batches <- ceiling(n_rows / batch_size)
  
  # Process in batches
  if (n_batches <= 1) {
    # Small dataset, process directly
    return(calculation_fn(dt))
  } else {
    # Process in batches and combine results
    results <- vector("list", n_batches)
    
    for (i in 1:n_batches) {
      start_idx <- (i - 1) * batch_size + 1
      end_idx <- min(i * batch_size, n_rows)
      batch <- dt[start_idx:end_idx, ]
      results[[i]] <- calculation_fn(batch)
    }
    
    # Combine results
    return(rbindlist(results))
  }
}

#' Optimized batting average calculation
#'
#' This function calculates batting averages for multiple players in a vectorized way.
#'
#' @param hits Vector of hits
#' @param at_bats Vector of at-bats
#' @param default Default value for invalid calculations
#' @return Vector of batting averages
optimized_batting_avg <- function(hits, at_bats, default = 0) {
  # Add input validation
  if (is.null(hits) || is.null(at_bats)) {

    return(default)
  }
  
  # Check for empty inputs
  if (length(hits) == 0 || length(at_bats) == 0) {

    return(default)
  }
  
  # Check for non-numeric inputs
  if (!is.numeric(hits) || !is.numeric(at_bats)) {

    return(default)
  }
  
  # Ensure inputs are valid numbers
  hits <- as.numeric(hits)
  at_bats <- as.numeric(at_bats)
  
  # Replace NA values with 0
  hits[is.na(hits)] <- 0
  at_bats[is.na(at_bats)] <- 0
  
  # Ensure at_bats is positive
  valid_indices <- at_bats > 0
  if (!any(valid_indices)) {

    return(default)
  }
  
  # Vectorized calculation
  result <- vectorized_safe_divide(hits, at_bats, default)
  
  # Ensure results are between 0 and 1
  result[result < 0] <- 0
  result[result > 1] <- 1
  
  return(result)
}

#' Optimized on-base percentage calculation
#'
#' This function calculates OBP for multiple players in a vectorized way.
#'
#' @param hits Vector of hits
#' @param walks Vector of walks
#' @param hbp Vector of hit-by-pitch
#' @param at_bats Vector of at-bats
#' @param sf Vector of sacrifice flies
#' @param default Default value for invalid calculations
#' @return Vector of on-base percentages
optimized_obp <- function(hits, walks, hbp, at_bats, sf, default = 0) {
  # Add input validation
  if (is.null(hits) || is.null(walks) || is.null(hbp) || is.null(at_bats) || is.null(sf)) {

    return(default)
  }
  
  # Check for empty inputs
  if (length(hits) == 0 || length(walks) == 0 || length(hbp) == 0 || 
      length(at_bats) == 0 || length(sf) == 0) {

    return(default)
  }
  
  # Check for non-numeric inputs
  if (!is.numeric(hits) || !is.numeric(walks) || !is.numeric(hbp) || 
      !is.numeric(at_bats) || !is.numeric(sf)) {

    return(default)
  }
  
  # Ensure inputs are valid numbers
  hits <- as.numeric(hits)
  walks <- as.numeric(walks)
  hbp <- as.numeric(hbp)
  at_bats <- as.numeric(at_bats)
  sf <- as.numeric(sf)
  
  # Replace NA values with 0
  hits[is.na(hits)] <- 0
  walks[is.na(walks)] <- 0
  hbp[is.na(hbp)] <- 0
  at_bats[is.na(at_bats)] <- 0
  sf[is.na(sf)] <- 0
  
  # Vectorized calculation
  numerator <- hits + walks + hbp
  denominator <- at_bats + walks + hbp + sf
  
  # Ensure denominator is positive
  valid_indices <- denominator > 0
  if (!any(valid_indices)) {

    return(default)
  }
  
  result <- vectorized_safe_divide(numerator, denominator, default)
  
  # Ensure results are between 0 and 1
  result[result < 0] <- 0
  result[result > 1] <- 1
  
  return(result)
}

#' Optimized slugging percentage calculation
#'
#' This function calculates SLG for multiple players in a vectorized way.
#'
#' @param total_bases Vector of total bases
#' @param at_bats Vector of at-bats
#' @param default Default value for invalid calculations
#' @return Vector of slugging percentages
optimized_slg <- function(total_bases, at_bats, default = 0) {
  # Add input validation
  if (is.null(total_bases) || is.null(at_bats)) {

    return(default)
  }
  
  # Check for empty inputs
  if (length(total_bases) == 0 || length(at_bats) == 0) {

    return(default)
  }
  
  # Check for non-numeric inputs
  if (!is.numeric(total_bases) || !is.numeric(at_bats)) {

    return(default)
  }
  
  # Ensure inputs are valid numbers
  total_bases <- as.numeric(total_bases)
  at_bats <- as.numeric(at_bats)
  
  # Replace NA values with 0
  total_bases[is.na(total_bases)] <- 0
  at_bats[is.na(at_bats)] <- 0
  
  # Ensure at_bats is positive
  valid_indices <- at_bats > 0
  if (!any(valid_indices)) {

    return(default)
  }
  
  # Vectorized calculation
  result <- vectorized_safe_divide(total_bases, at_bats, default)
  
  # Ensure results are non-negative
  result[result < 0] <- 0
  
  return(result)
}

#' Optimized ERA calculation
#'
#' This function calculates ERA for multiple pitchers in a vectorized way.
#'
#' @param earned_runs Vector of earned runs
#' @param innings_pitched Vector of innings pitched
#' @param default Default value for invalid calculations
#' @param max_era Maximum ERA value to return
#' @return Vector of ERAs
optimized_era <- function(earned_runs, innings_pitched, default = 9.99, max_era = 99.99) {
  # Vectorized calculation
  result <- vectorized_safe_divide(9 * earned_runs, innings_pitched, default)
  
  # Cap extremely high ERAs
  result[result > max_era] <- max_era
  
  return(result)
}

#' Optimized WHIP calculation
#'
#' This function calculates WHIP for multiple pitchers in a vectorized way.
#'
#' @param walks Vector of walks
#' @param hits Vector of hits
#' @param innings_pitched Vector of innings pitched
#' @param default Default value for invalid calculations
#' @param max_whip Maximum WHIP value to return
#' @return Vector of WHIPs
optimized_whip <- function(walks, hits, innings_pitched, default = 2.00, max_whip = 9.99) {
  # Vectorized calculation
  result <- vectorized_safe_divide(walks + hits, innings_pitched, default)
  
  # Cap extremely high WHIPs
  result[result > max_whip] <- max_whip
  
  return(result)
}

#' Optimized per-9 rate calculation
#'
#' This function calculates rates per 9 innings for multiple pitchers in a vectorized way.
#'
#' @param value Vector of values (e.g., strikeouts, walks)
#' @param innings_pitched Vector of innings pitched
#' @param default Default value for invalid calculations
#' @return Vector of rates per 9 innings
optimized_per_nine <- function(value, innings_pitched, default = 0) {
  # Vectorized calculation
  return(vectorized_safe_divide(9 * value, innings_pitched, default))
}

#' Calculate league averages efficiently
#'
#' This function calculates league averages more efficiently using data.table.
#'
#' @param batting_data Data frame of batting statistics
#' @param pitching_data Data frame of pitching statistics
#' @return List of league averages
calculate_league_averages_optimized <- function(batting_data, pitching_data) {
  # Check if batting_data and pitching_data are valid
  if (is.null(batting_data) || nrow(batting_data) == 0) {

    # Return default values
    return(list(
      batting = data.frame(
        League_AVG = 0.250,
        League_OBP = 0.320,
        League_SLG = 0.400,
        League_OPS = 0.720,
        League_TB_per_PA = 0.5,
        League_BB_per_PA = 0.1,
        League_SB_per_PA = 0.05,
        League_PA_per_Game = 4.5
      ),
      pitching = data.frame(
        League_ERA = 4.50,
        League_WHIP = 1.30,
        League_K_per_IP = 1.0,
        League_BB_per_IP = 0.5,
        League_HR_per_IP = 0.1,
        League_ER_per_IP = 1.0,
        League_FIP_Constant = 3.10
      )
    ))
  }
  
  # Convert to data.table for faster processing

  batting_dt <- tryCatch({
    as.data.table(batting_data)
  }, error = function(e) {

    return(data.table())
  })
  
  pitching_dt <- tryCatch({
    as.data.table(pitching_data)
  }, error = function(e) {

    return(data.table())
  })
  
  # Check if conversion was successful
  if (nrow(batting_dt) == 0) {

    # Return default values
    return(list(
      batting = data.frame(
        League_AVG = 0.250,
        League_OBP = 0.320,
        League_SLG = 0.400,
        League_OPS = 0.720,
        League_TB_per_PA = 0.5,
        League_BB_per_PA = 0.1,
        League_SB_per_PA = 0.05,
        League_PA_per_Game = 4.5
      ),
      pitching = data.frame(
        League_ERA = 4.50,
        League_WHIP = 1.30,
        League_K_per_IP = 1.0,
        League_BB_per_IP = 0.5,
        League_HR_per_IP = 0.1,
        League_ER_per_IP = 1.0,
        League_FIP_Constant = 3.10
      )
    ))
  }
  
  # Calculate batting averages with error handling

  batting_sums <- tryCatch({
    batting_dt[, .(
      Games = uniqueN(GameID),
      TotalPA = sum(PA, na.rm = TRUE),
      TotalAB = sum(PA - BB - HBP - SF - SH, na.rm = TRUE),
      TotalHits = sum(`1B` + `2B` + `3B` + HR, na.rm = TRUE),
      Singles = sum(`1B`, na.rm = TRUE),
      Doubles = sum(`2B`, na.rm = TRUE),
      Triples = sum(`3B`, na.rm = TRUE),
      HRs = sum(HR, na.rm = TRUE),
      Walks = sum(BB, na.rm = TRUE),
      HitByPitch = sum(HBP, na.rm = TRUE),
      SacrificeFly = sum(SF, na.rm = TRUE),
      StolenBases = sum(SB, na.rm = TRUE),
      RBI = sum(RBI, na.rm = TRUE)
    )]
  }, error = function(e) {

    # Return a default data.table
    data.table(
      Games = 9,
      TotalPA = 324,
      TotalAB = 270,
      TotalHits = 67.5,
      Singles = 45,
      Doubles = 13.5,
      Triples = 4.5,
      HRs = 4.5,
      Walks = 27,
      HitByPitch = 9,
      SacrificeFly = 9,
      StolenBases = 13.5,
      RBI = 40.5
    )
  })
  
  # Ensure non-zero values for division with error handling

  tryCatch({
    batting_sums[, `:=`(
      Games = pmax(Games * 2, 1),  # Both teams play each game
      TotalPA = pmax(TotalPA, 1),
      TotalAB = pmax(TotalAB, 1)
    )]
  }, error = function(e) {

    # Try manual assignment if data.table syntax fails
    batting_sums$Games <- pmax(batting_sums$Games * 2, 1)
    batting_sums$TotalPA <- pmax(batting_sums$TotalPA, 1)
    batting_sums$TotalAB <- pmax(batting_sums$TotalAB, 1)
  })
  
  # Calculate derived stats with error handling

  tb <- tryCatch({
    batting_sums$Singles + (2 * batting_sums$Doubles) + 
    (3 * batting_sums$Triples) + (4 * batting_sums$HRs)
  }, error = function(e) {

    return(67.5) # Default value
  })
  
  avg <- tryCatch({
    optimized_batting_avg(batting_sums$TotalHits, batting_sums$TotalAB)[1]
  }, error = function(e) {

    return(0.250) # Default value
  })
  
  obp <- tryCatch({
    optimized_obp(
      batting_sums$TotalHits, 
      batting_sums$Walks, 
      batting_sums$HitByPitch, 
      batting_sums$TotalAB, 
      batting_sums$SacrificeFly
    )[1]
  }, error = function(e) {

    return(0.320) # Default value
  })
  
  slg <- tryCatch({
    optimized_slg(tb, batting_sums$TotalAB)[1]
  }, error = function(e) {

    return(0.400) # Default value
  })
  
  ops <- obp + slg
  
  # Create league batting averages with error handling

  league_batting <- tryCatch({
    data.frame(
      League_AVG = avg,
      League_OBP = obp,
      League_SLG = slg,
      League_OPS = ops,
      League_TB_per_PA = tb / batting_sums$TotalPA,
      League_BB_per_PA = batting_sums$Walks / batting_sums$TotalPA,
      League_SB_per_PA = batting_sums$StolenBases / batting_sums$TotalPA,
      League_PA_per_Game = batting_sums$TotalPA / batting_sums$Games
    )
  }, error = function(e) {

    # Return default values
    data.frame(
      League_AVG = 0.250,
      League_OBP = 0.320,
      League_SLG = 0.400,
      League_OPS = 0.720,
      League_TB_per_PA = 0.5,
      League_BB_per_PA = 0.1,
      League_SB_per_PA = 0.05,
      League_PA_per_Game = 4.5
    )
  })
  
  # Calculate pitching averages if data exists

  tryCatch({
    if (!is.null(pitching_dt) && nrow(pitching_dt) > 0) {


      
      # Try with OutsRecorded first
      pitching_sums <- tryCatch({
        if ("OutsRecorded" %in% names(pitching_dt)) {

          pitching_dt[, .(
            IP = sum(OutsRecorded, na.rm = TRUE) / 3.0,
            ER = sum(ER, na.rm = TRUE),
            Hits = sum(H, na.rm = TRUE),
            Walks = sum(BB, na.rm = TRUE),
            Strikeouts = sum(K, na.rm = TRUE),
            HomeRuns = sum(HR, na.rm = TRUE),
            Games = uniqueN(GameID)
          )]
        } else if ("OR" %in% names(pitching_dt)) {

          # Try with OR if OutsRecorded doesn't exist
          pitching_dt[, .(
            IP = sum(OR, na.rm = TRUE) / 3.0,
            ER = sum(ER, na.rm = TRUE),
            Hits = sum(H, na.rm = TRUE),
            Walks = sum(BB, na.rm = TRUE),
            Strikeouts = sum(K, na.rm = TRUE),
            HomeRuns = sum(HR, na.rm = TRUE),
            Games = uniqueN(GameID)
          )]
        } else {

          # Default values if neither exists
          data.table(
            IP = 81,
            ER = 40.5,
            Hits = 81,
            Walks = 40.5,
            Strikeouts = 81,
            HomeRuns = 9,
            Games = 9
          )
        }
      }, error = function(e) {

        # Return default values
        data.table(
          IP = 81,
          ER = 40.5,
          Hits = 81,
          Walks = 40.5,
          Strikeouts = 81,
          HomeRuns = 9,
          Games = 9
        )
      })
      
      # Ensure non-zero values for division
      tryCatch({
        pitching_sums[, IP := pmax(IP, 1)]
      }, error = function(e) {

        # Try manual assignment
        pitching_sums$IP <- pmax(pitching_sums$IP, 1)
      })
      
      # Calculate derived stats with error handling

      era <- tryCatch({
        optimized_era(pitching_sums$ER, pitching_sums$IP)[1]
      }, error = function(e) {

        return(4.50) # Default value
      })
      
      whip <- tryCatch({
        optimized_whip(pitching_sums$Walks, pitching_sums$Hits, pitching_sums$IP)[1]
      }, error = function(e) {

        return(1.30) # Default value
      })
      
      k_per_ip <- tryCatch({
        optimized_per_nine(pitching_sums$Strikeouts, pitching_sums$IP)[1]
      }, error = function(e) {

        return(1.0) # Default value
      })
      
      bb_per_ip <- tryCatch({
        optimized_per_nine(pitching_sums$Walks, pitching_sums$IP)[1]
      }, error = function(e) {

        return(0.5) # Default value
      })
      
      hr_per_ip <- tryCatch({
        optimized_per_nine(pitching_sums$HomeRuns, pitching_sums$IP)[1]
      }, error = function(e) {

        return(0.1) # Default value
      })
      
      # Calculate FIP constant with error handling
      fip <- tryCatch({
        ((13 * pitching_sums$HomeRuns) + (3 * pitching_sums$Walks) - 
          (2 * pitching_sums$Strikeouts)) / pitching_sums$IP + 3.10
      }, error = function(e) {

        return(4.20) # Default value
      })
      
      fip_constant <- tryCatch({
        era - fip
      }, error = function(e) {

        return(3.10) # Default value
      })
      
      # Create league pitching averages with error handling
      cat("Creating league pitching averages data frame...\n")
      league_pitching <- tryCatch({
        data.frame(
          League_ERA = era,
          League_WHIP = whip,
          League_K_per_IP = k_per_ip,
          League_BB_per_IP = bb_per_ip,
          League_HR_per_IP = hr_per_ip,
          League_ER_per_IP = pitching_sums$ER / pitching_sums$IP,
          League_FIP_Constant = fip_constant
        )
      }, error = function(e) {
        cat("ERROR creating pitching averages data frame:", e$message, "\n")
        # Return default values
        data.frame(
          League_ERA = 4.50,
          League_WHIP = 1.30,
          League_K_per_IP = 1.0,
          League_BB_per_IP = 0.5,
          League_HR_per_IP = 0.1,
          League_ER_per_IP = 1.0,
          League_FIP_Constant = 3.10
        )
      })
    } else {
      cat("No pitching data available, using default values\n")
      # Default values if no pitching data
      league_pitching <- data.frame(
        League_ERA = 4.50,
        League_WHIP = 1.30,
        League_K_per_IP = 1.0,
        League_BB_per_IP = 0.5,
        League_HR_per_IP = 0.1,
        League_ER_per_IP = 1.0,
        League_FIP_Constant = 3.10
      )
    }
  }, error = function(e) {
    cat("ERROR in pitching calculations:", e$message, "\n")
    # Default values if error in entire pitching section
    league_pitching <- data.frame(
      League_ERA = 4.50,
      League_WHIP = 1.30,
      League_K_per_IP = 1.0,
      League_BB_per_IP = 0.5,
      League_HR_per_IP = 0.1,
      League_ER_per_IP = 1.0,
      League_FIP_Constant = 3.10
    )
  })
  
  # Return both sets of averages
  return(list(
    batting = league_batting,
    pitching = league_pitching
  ))
}

#' Calculate player statistics efficiently
#'
#' This function calculates player statistics more efficiently using data.table.
#'
#' @param player_data Data frame of player statistics
#' @param league_averages League averages
#' @param stat_type Type of statistics ("batting" or "pitching")
#' @return Data frame of calculated statistics
calculate_player_stats_optimized <- function(player_data, league_averages, stat_type = "batting") {
  # Convert to data.table for faster processing
  dt <- as.data.table(player_data)
  
  if (stat_type == "batting") {
    # Group by PlayerID
    player_stats <- dt[, .(
      PA = sum(PA, na.rm = TRUE),
      AB = sum(PA - BB - HBP - SF - SH, na.rm = TRUE),
      Hits = sum(`1B` + `2B` + `3B` + HR, na.rm = TRUE),
      Singles = sum(`1B`, na.rm = TRUE),
      Doubles = sum(`2B`, na.rm = TRUE),
      Triples = sum(`3B`, na.rm = TRUE),
      HomeRuns = sum(HR, na.rm = TRUE),
      Walks = sum(BB, na.rm = TRUE),
      HBP = sum(HBP, na.rm = TRUE),
      SF = sum(SF, na.rm = TRUE),
      SB = sum(SB, na.rm = TRUE),
      RBI = sum(RBI, na.rm = TRUE),
      Games = uniqueN(GameID)
    ), by = PlayerID]
    
    # Calculate total bases
    player_stats[, TB := Singles + (2 * Doubles) + (3 * Triples) + (4 * HomeRuns)]
    
    # Calculate rate stats
    player_stats[, `:=`(
      AVG = optimized_batting_avg(Hits, AB),
      OBP = optimized_obp(Hits, Walks, HBP, AB, SF),
      SLG = optimized_slg(TB, AB)
    )]
    
    # Calculate OPS
    player_stats[, OPS := OBP + SLG]
    
    # Calculate advanced metrics
    league_ops <- league_averages$batting$League_OPS
    player_stats[, OPS_Plus := round(vectorized_safe_divide(100 * OPS, league_ops, 100))]
    
    # Calculate WAR components
    league_tb_per_pa <- league_averages$batting$League_TB_per_PA
    league_bb_per_pa <- league_averages$batting$League_BB_per_PA
    league_sb_per_pa <- league_averages$batting$League_SB_per_PA
    
    player_stats[, `:=`(
      hitting_value = (TB - (PA * league_tb_per_pa)) * 0.6,
      on_base_value = (Walks - (PA * league_bb_per_pa)) * 0.4,
      baserunning_value = (SB - (PA * league_sb_per_pa)) * 0.2
    )]
    
    player_stats[, WAR := round(vectorized_safe_divide(
      hitting_value + on_base_value + baserunning_value, 15, 0) * 5.0, 1)]
    
    return(player_stats)
    
  } else if (stat_type == "pitching") {
    # Determine which column to use for outs
    outs_col <- if ("OutsRecorded" %in% names(dt)) "OutsRecorded" else "OR"
    
    # Group by PlayerID
    player_stats <- dt[, .(
      IP = sum(get(outs_col), na.rm = TRUE) / 3.0,
      ER = sum(ER, na.rm = TRUE),
      Hits = sum(H, na.rm = TRUE),
      Walks = sum(BB, na.rm = TRUE),
      Strikeouts = sum(K, na.rm = TRUE),
      HomeRuns = sum(HR, na.rm = TRUE),
      Games = uniqueN(GameID)
    ), by = PlayerID]
    
    # Calculate rate stats
    player_stats[, `:=`(
      ERA = optimized_era(ER, IP),
      WHIP = optimized_whip(Walks, Hits, IP),
      K9 = optimized_per_nine(Strikeouts, IP),
      BB9 = optimized_per_nine(Walks, IP),
      HR9 = optimized_per_nine(HomeRuns, IP)
    )]
    
    # Calculate advanced metrics
    league_era <- league_averages$pitching$League_ERA
    player_stats[, ERA_Plus := round(vectorized_safe_divide(100 * league_era, ERA, 100))]
    
    # Calculate FIP
    fip_constant <- league_averages$pitching$League_FIP_Constant
    player_stats[, FIP := round(vectorized_safe_divide(
      (13 * HomeRuns) + (3 * Walks) - (2 * Strikeouts), IP, 4.50) + fip_constant, 2)]
    
    # Calculate WAR components
    league_k_per_ip <- league_averages$pitching$League_K_per_IP
    league_bb_per_ip <- league_averages$pitching$League_BB_per_IP
    league_hr_per_ip <- league_averages$pitching$League_HR_per_IP
    league_er_per_ip <- league_averages$pitching$League_ER_per_IP
    
    player_stats[, `:=`(
      strikeout_value = (Strikeouts - (IP * league_k_per_ip)) * 0.5,
      walk_value = -(Walks - (IP * league_bb_per_ip)) * 0.4,
      hr_value = -(HomeRuns - (IP * league_hr_per_ip)) * 1.2,
      run_prevention_value = -(ER - (IP * league_er_per_ip)) * 0.8
    )]
    
    player_stats[, WAR := round(vectorized_safe_divide(
      strikeout_value + walk_value + hr_value + run_prevention_value, 15, 0) * 5.0, 1)]
    
    return(player_stats)
  }
  
  return(NULL)
}

#' Efficiently calculate player rankings
#'
#' This function calculates player rankings more efficiently.
#'
#' @param player_stats Data frame of player statistics
#' @param team_id Team ID to filter by (optional)
#' @param min_pa Minimum plate appearances to qualify (for batting)
#' @param min_ip Minimum innings pitched to qualify (for pitching)
#' @param stat_type Type of statistics ("batting" or "pitching")
#' @return Data frame of player rankings
calculate_player_rankings_optimized <- function(player_stats, team_id = NULL, 
                                              min_pa = 10, min_ip = 3, 
                                              stat_type = "batting") {
  # Convert to data.table for faster processing
  dt <- as.data.table(player_stats)
  
  # Filter by team if specified
  if (!is.null(team_id)) {
    dt <- dt[TeamID == team_id]
  }
  
  if (stat_type == "batting") {
    # Filter by minimum PA
    qualified <- dt[PA >= min_pa]
    
    if (nrow(qualified) > 0) {
      # Calculate rankings for each stat
      qualified[, AVG_Rank := rank(-AVG, ties.method = "min")]
      qualified[, OBP_Rank := rank(-OBP, ties.method = "min")]
      qualified[, SLG_Rank := rank(-SLG, ties.method = "min")]
      qualified[, OPS_Rank := rank(-OPS, ties.method = "min")]
      qualified[, HR_Rank := rank(-HomeRuns, ties.method = "min")]
      qualified[, RBI_Rank := rank(-RBI, ties.method = "min")]
      qualified[, WAR_Rank := rank(-WAR, ties.method = "min")]
      
      return(qualified)
    }
  } else if (stat_type == "pitching") {
    # Filter by minimum IP
    qualified <- dt[IP >= min_ip]
    
    if (nrow(qualified) > 0) {
      # Calculate rankings for each stat (note: lower ERA/WHIP is better)
      qualified[, ERA_Rank := rank(ERA, ties.method = "min")]
      qualified[, WHIP_Rank := rank(WHIP, ties.method = "min")]
      qualified[, K_Rank := rank(-Strikeouts, ties.method = "min")]
      qualified[, K9_Rank := rank(-K9, ties.method = "min")]
      qualified[, WAR_Rank := rank(-WAR, ties.method = "min")]
      
      return(qualified)
    }
  }
  
  return(NULL)
}

#' Efficiently project full season stats
#'
#' This function projects full season statistics more efficiently.
#'
#' @param stats Data frame of player statistics
#' @param games_played Number of games played
#' @param season_length Full season length
#' @param stat_type Type of statistics ("batting" or "pitching")
#' @return Data frame of projected statistics
project_full_season_optimized <- function(stats, games_played, season_length = 162, 
                                        stat_type = "batting") {
  # Return as-is if no games played
  if (games_played <= 0) return(stats)
  
  # Calculate scale factor
  scale_factor <- season_length / games_played
  
  # Convert to data.table for faster processing
  dt <- as.data.table(copy(stats))
  
  if (stat_type == "batting") {
    # Define counting stats to scale
    counting_cols <- c("PA", "AB", "Hits", "Singles", "Doubles", "Triples", "HomeRuns", 
                      "Walks", "HBP", "SF", "SB", "RBI", "TB")
    
    # Scale only columns that exist
    existing_cols <- intersect(counting_cols, names(dt))
    
    # Scale the counting stats
    for (col in existing_cols) {
      dt[, (col) := round(get(col) * scale_factor, 0)]
    }
    
    # Recalculate rate stats
    if (all(c("Hits", "AB") %in% names(dt))) {
      dt[, AVG := optimized_batting_avg(Hits, AB)]
    }
    
    if (all(c("Hits", "Walks", "HBP", "AB", "SF") %in% names(dt))) {
      dt[, OBP := optimized_obp(Hits, Walks, HBP, AB, SF)]
    }
    
    if (all(c("TB", "AB") %in% names(dt))) {
      dt[, SLG := optimized_slg(TB, AB)]
    }
    
    if (all(c("OBP", "SLG") %in% names(dt))) {
      dt[, OPS := OBP + SLG]
    }
    
  } else if (stat_type == "pitching") {
    # Define counting stats to scale
    counting_cols <- c("IP", "ER", "Hits", "Walks", "Strikeouts", "HomeRuns")
    
    # Scale only columns that exist
    existing_cols <- intersect(counting_cols, names(dt))
    
    # Scale the counting stats
    for (col in existing_cols) {
      dt[, (col) := round(get(col) * scale_factor, 0)]
    }
    
    # Recalculate rate stats
    if (all(c("ER", "IP") %in% names(dt))) {
      dt[, ERA := optimized_era(ER, IP)]
    }
    
    if (all(c("Walks", "Hits", "IP") %in% names(dt))) {
      dt[, WHIP := optimized_whip(Walks, Hits, IP)]
    }
    
    if (all(c("Strikeouts", "IP") %in% names(dt))) {
      dt[, K9 := optimized_per_nine(Strikeouts, IP)]
    }
    
    if (all(c("Walks", "IP") %in% names(dt))) {
      dt[, BB9 := optimized_per_nine(Walks, IP)]
    }
    
    if (all(c("HomeRuns", "IP") %in% names(dt))) {
      dt[, HR9 := optimized_per_nine(HomeRuns, IP)]
    }
  }
  
  return(dt)
}

#' Cache results of expensive calculations
#'
#' This function creates a caching mechanism for expensive calculations.
#'
#' @param key Cache key
#' @param calculation_fn Function to perform the calculation
#' @param cache_duration Duration in seconds to keep the cache valid
#' @param force_refresh Whether to force a refresh of the cache
#' @return Result of the calculation
cache_result <- function(key, calculation_fn, cache_duration = 300, force_refresh = FALSE) {
  # Create cache environment if it doesn't exist
  if (!exists("stats_cache", envir = .GlobalEnv)) {
    assign("stats_cache", new.env(), envir = .GlobalEnv)
  }
  
  cache <- get("stats_cache", envir = .GlobalEnv)
  cache_key <- paste0(key, "_result")
  timestamp_key <- paste0(key, "_timestamp")
  
  current_time <- as.numeric(Sys.time())
  
  # Check if we need to refresh the cache
  need_refresh <- force_refresh || 
                 !exists(cache_key, envir = cache) || 
                 !exists(timestamp_key, envir = cache) ||
                 (current_time - get(timestamp_key, envir = cache) > cache_duration)
  
  if (need_refresh) {
    # Calculate and store in cache
    result <- calculation_fn()
    assign(cache_key, result, envir = cache)
    assign(timestamp_key, current_time, envir = cache)
    return(result)
  } else {
    # Return cached result
    return(get(cache_key, envir = cache))
  }
}

#' Clear the statistics cache
#'
#' This function clears the statistics cache.
#'
#' @param key Specific cache key to clear (optional)
#' @return NULL
clear_cache <- function(key = NULL) {
  if (!exists("stats_cache", envir = .GlobalEnv)) {
    return(NULL)
  }
  
  cache <- get("stats_cache", envir = .GlobalEnv)
  
  if (is.null(key)) {
    # Clear entire cache
    rm(list = ls(envir = cache), envir = cache)
  } else {
    # Clear specific key
    cache_key <- paste0(key, "_result")
    timestamp_key <- paste0(key, "_timestamp")
    
    if (exists(cache_key, envir = cache)) {
      rm(list = cache_key, envir = cache)
    }
    
    if (exists(timestamp_key, envir = cache)) {
      rm(list = timestamp_key, envir = cache)
    }
  }
  
  return(NULL)
}
