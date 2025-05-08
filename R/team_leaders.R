# team_leaders.R
# Functions for retrieving team leaders in various statistical categories

# Load required utilities
source("R/math_utils.R")
source("R/error_handling.R")
source("R/object_engine.R")
library(shiny)

# Declare global variables to avoid lint warnings
utils::globalVariables(c(
  # Data frames
  "teams", "players", "batting_stats", "pitching_stats",
  
  # Functions
  "safe_divide", "safe_batting_avg", "safe_obp", "safe_slg", "safe_era", "safe_whip", "safe_per_nine",
  
  # Variables
  "league_avgs"
))


#' Get team leaders in various statistical categories
#' 
#' @param team_id Team ID
#' @param db_function Database function
#' @return List of dataframes with team leaders in different categories
get_team_leaders <- function(team_id, teams_df = teams, players_df = players, batting_stats_df = batting_stats, pitching_stats_df = pitching_stats) {
  # Initialize result with empty data frames
  result <- list(
    BA = data.frame(PlayerID = integer(0), DisplayName = character(0), BA = numeric(0), stringsAsFactors = FALSE),
    OPS = data.frame(PlayerID = integer(0), DisplayName = character(0), OPS = numeric(0), stringsAsFactors = FALSE),
    OPS_Plus = data.frame(PlayerID = integer(0), DisplayName = character(0), OPS_Plus = numeric(0), stringsAsFactors = FALSE),
    bWAR = data.frame(PlayerID = integer(0), DisplayName = character(0), bWAR = numeric(0), stringsAsFactors = FALSE),
    ERA_Plus = data.frame(PlayerID = integer(0), DisplayName = character(0), ERA_Plus = numeric(0), stringsAsFactors = FALSE),
    pWAR = data.frame(PlayerID = integer(0), DisplayName = character(0), pWAR = numeric(0), stringsAsFactors = FALSE)
  )
  
  # Wrap everything in a tryCatch to prevent errors from breaking the app
  tryCatch({

    team_id_numeric <- as.numeric(team_id)
    
    # Get batting leaders using in-memory data frames
    batting_stats_team <- batting_stats_df[batting_stats_df$TeamID == team_id_numeric, ]
    players_team <- players_df[players_df$TeamID == team_id_numeric, ]
    if (nrow(players_team) == 0) return(result)
    
    batting_leaders <- lapply(seq_len(nrow(players_team)), function(i) {
      player <- players_team[i, ]
      stats <- batting_stats_team[batting_stats_team$PlayerID == player$PlayerID, ]
      PA <- sum(stats$PA, na.rm = TRUE)
      BB <- sum(stats$BB, na.rm = TRUE)
      HBP <- sum(stats$HBP, na.rm = TRUE)
      SF <- sum(stats$SF, na.rm = TRUE)
      SH <- sum(stats$SH, na.rm = TRUE)
      AB <- PA - BB - HBP - SF - SH
      singles <- sum(stats$X1B, na.rm = TRUE)
      doubles <- sum(stats$X2B, na.rm = TRUE)
      triples <- sum(stats$X3B, na.rm = TRUE)
      HR <- sum(stats$HR, na.rm = TRUE)
      Hits <- singles + doubles + triples + HR
      OBP <- safe_obp(Hits, BB, HBP, AB, SF)
      SLG <- safe_slg(singles + 2*doubles + 3*triples + 4*HR, AB)
      BA <- safe_batting_avg(Hits, AB)
      OPS <- OBP + SLG
      list(PlayerID = player$PlayerID, DisplayName = paste(player$FirstName, player$LastInitial, "."), BA = BA, OBP = OBP, SLG = SLG, PA = PA, OPS = OPS)
    })
    batting_leaders <- do.call(rbind, lapply(batting_leaders, as.data.frame))
    batting_leaders <- batting_leaders[order(-batting_leaders$BA), ]
    
    # Calculate OPS+
    league_avg_ops <- 0.700  # Default value
    batting_leaders$OPS_Plus <- round(100 * batting_leaders$OPS / league_avg_ops)
    
    # Calculate simple WAR approximation for youth baseball
    batting_leaders$bWAR <- round((batting_leaders$OPS - league_avg_ops) * batting_leaders$PA / 20, 1)
    
    # Sort and assign to result if there are rows
    if (nrow(batting_leaders) > 0) {
      # Ensure all required columns exist
      if (all(c("PlayerID", "DisplayName", "BA") %in% names(batting_leaders))) {
        result$BA <- batting_leaders[order(-batting_leaders$BA), c("PlayerID", "DisplayName", "BA")]
      }
      if (all(c("PlayerID", "DisplayName", "OPS") %in% names(batting_leaders))) {
        result$OPS <- batting_leaders[order(-batting_leaders$OPS), c("PlayerID", "DisplayName", "OPS")]
      }
      if (all(c("PlayerID", "DisplayName", "OPS_Plus") %in% names(batting_leaders))) {
        result$OPS_Plus <- batting_leaders[order(-batting_leaders$OPS_Plus), c("PlayerID", "DisplayName", "OPS_Plus")]
      }
      if (all(c("PlayerID", "DisplayName", "bWAR") %in% names(batting_leaders))) {
        result$bWAR <- batting_leaders[order(-batting_leaders$bWAR), c("PlayerID", "DisplayName", "bWAR")]
      }
    }
    
    # Get pitching leaders
    pitching_stats_team <- pitching_stats_df[pitching_stats_df$TeamID == team_id_numeric, ]
    pitching_leaders <- lapply(seq_len(nrow(players_team)), function(i) {
      player <- players_team[i, ]
      stats <- pitching_stats_team[pitching_stats_team$PlayerID == player$PlayerID, ]
      OutsRecorded <- sum(stats$OutsRecorded, na.rm = TRUE)
      IP <- OutsRecorded / 3
      ER <- sum(stats$ER, na.rm = TRUE)
      BB <- sum(stats$BB, na.rm = TRUE)
      H <- sum(stats$H, na.rm = TRUE)
      K <- sum(stats$K, na.rm = TRUE)
      HR <- sum(stats$HR, na.rm = TRUE)
      HBP <- sum(stats$HBP, na.rm = TRUE)
      BF <- sum(stats$BF, na.rm = TRUE)
      ERA <- safe_era(ER, IP)
      WHIP <- safe_whip(BB, H, IP, HBP)
      K_per_9 <- safe_per_nine(K, IP)
      BB_per_9 <- safe_per_nine(BB, IP)
      HR_per_9 <- safe_per_nine(HR, IP)
      K_pct <- safe_divide(K, BF)
      BB_pct <- safe_divide(BB, BF)
      list(PlayerID = player$PlayerID, DisplayName = paste(player$FirstName, player$LastInitial, "."), ERA = ERA, WHIP = WHIP, IP = IP)
    })
    pitching_leaders <- do.call(rbind, lapply(pitching_leaders, as.data.frame))
    pitching_leaders <- pitching_leaders[order(pitching_leaders$ERA), ]
    
    # Process pitching leaders

    # Process pitching leaders data if available
    
    if (!is.null(pitching_leaders) && is.data.frame(pitching_leaders) && nrow(pitching_leaders) > 0) {
      # Handle NA values in ERA and WHIP
      pitching_leaders$ERA <- ifelse(is.na(pitching_leaders$ERA), 9999, as.numeric(pitching_leaders$ERA))
      pitching_leaders$WHIP <- ifelse(is.na(pitching_leaders$WHIP), 9999, as.numeric(pitching_leaders$WHIP))
      pitching_leaders$IP <- ifelse(is.na(pitching_leaders$IP), 0, as.numeric(pitching_leaders$IP))
      
      # Get league average ERA for ERA+
      league_avg_era <- 4.50  # Default value
      # Get league averages for ERA+ calculation
      league_avgs <- tryCatch({
        get_league_averages()
      }, error = function(e) {
        # Use default values if league averages can't be retrieved
        return(list(pitching = list(League_ERA = 4.50)))
      })
      
      if (!is.null(league_avgs) && is.list(league_avgs) && 
          !is.null(league_avgs$pitching) && is.list(league_avgs$pitching) && 
          !is.null(league_avgs$pitching$League_ERA)) {
        league_avg_era <- league_avgs$pitching$League_ERA
      }
      
      # Calculate ERA+ with safe handling of edge cases
      tryCatch({
        pitching_leaders$ERA_Plus <- round(100 * league_avg_era / pmax(pitching_leaders$ERA, 0.01))
      }, error = function(e) {
        # Use default value if ERA+ calculation fails
        pitching_leaders$ERA_Plus <- 100  # Default value
      })
      
      # Calculate simple pitching WAR approximation for youth baseball with safe handling
      tryCatch({
        pitching_leaders$pWAR <- round((league_avg_era - pitching_leaders$ERA) * pitching_leaders$IP / 9, 1)
      }, error = function(e) {
        # Use default value if pWAR calculation fails
        pitching_leaders$pWAR <- 0  # Default value
      })
      
      # Sort and assign to result with safe handling
      tryCatch({
        if (nrow(pitching_leaders) > 0) {
          if (all(c("PlayerID", "DisplayName", "ERA_Plus") %in% names(pitching_leaders))) {
            result$ERA_Plus <- pitching_leaders[order(-pitching_leaders$ERA_Plus), c("PlayerID", "DisplayName", "ERA_Plus")]
          }
          if (all(c("PlayerID", "DisplayName", "pWAR") %in% names(pitching_leaders))) {
            result$pWAR <- pitching_leaders[order(-pitching_leaders$pWAR), c("PlayerID", "DisplayName", "pWAR")]
          }
        }
      }, error = function(e) {
        # Handle errors when sorting pitching leaders
      })
    }
    
  }, error = function(e) {
    # Handle any errors in the get_team_leaders function
  })
  
  return(result)
}

#' Get league average statistics
#' 
#' @return List of league average statistics
get_league_averages <- function() {
  # Initialize result
  result <- list(
    batting = list(
      League_AVG = 0.250,
      League_OBP = 0.320,
      League_SLG = 0.380,
      League_OPS = 0.700
    ),
    pitching = list(
      League_ERA = 4.50,
      League_WHIP = 1.30,
      League_FIP_Constant = 3.10
    )
  )
  
  # Calculate league averages from in-memory data frames
  tryCatch({
    # Get batting averages using object_engine.R functions when possible
    batting_avgs <- tryCatch({
      # Try to use the get_league_averages function if it exists
      if (exists("get_league_averages", mode = "function")) {
        league_avgs <- get_league_averages()
        if (is.list(league_avgs) && !is.null(league_avgs$batting)) {
          return(league_avgs$batting)
        }
      }
      
      # Fallback to manual calculation
      if (exists("batting_stats", envir = .GlobalEnv)) {
        bs <- get("batting_stats", envir = .GlobalEnv)
        ab <- sum(bs$PA - bs$BB - bs$HBP - bs$SF - bs$SH, na.rm = TRUE)
        hits <- sum(bs$X1B + bs$X2B + bs$X3B + bs$HR, na.rm = TRUE)
        obp_num <- hits + sum(bs$BB, na.rm = TRUE) + sum(bs$HBP, na.rm = TRUE)
        obp_den <- ab + sum(bs$BB, na.rm = TRUE) + sum(bs$HBP, na.rm = TRUE) + sum(bs$SF, na.rm = TRUE)
        slg_num <- sum(bs$X1B, na.rm = TRUE) + 2 * sum(bs$X2B, na.rm = TRUE) + 3 * sum(bs$X3B, na.rm = TRUE) + 4 * sum(bs$HR, na.rm = TRUE)
        avg <- safe_batting_avg(hits, ab)
        obp <- safe_obp(hits, sum(bs$BB, na.rm = TRUE), sum(bs$HBP, na.rm = TRUE), ab, sum(bs$SF, na.rm = TRUE))
        slg <- safe_slg(slg_num, ab)
        data.frame(League_AVG = avg, League_OBP = obp, League_SLG = slg)
      } else {
        data.frame(League_AVG = 0.250, League_OBP = 0.320, League_SLG = 0.380)
      }
    }, error = function(e) {
      # Use default values if batting averages can't be calculated
      return(data.frame(
        League_AVG = 0.250,
        League_OBP = 0.320,
        League_SLG = 0.380
      ))
    })
    
    if (!is.null(batting_avgs) && is.data.frame(batting_avgs) && nrow(batting_avgs) > 0) {
      # Use safer conversion with explicit NA checking
      result$batting$League_AVG <- ifelse(is.na(batting_avgs$League_AVG[1]), 0.250, as.numeric(batting_avgs$League_AVG[1]))
      result$batting$League_OBP <- ifelse(is.na(batting_avgs$League_OBP[1]), 0.320, as.numeric(batting_avgs$League_OBP[1]))
      result$batting$League_SLG <- ifelse(is.na(batting_avgs$League_SLG[1]), 0.380, as.numeric(batting_avgs$League_SLG[1]))
      result$batting$League_OPS <- result$batting$League_OBP + result$batting$League_SLG
    }
    
    # Get pitching averages
    pitching_avgs <- tryCatch({
      # Calculate from in-memory pitching_stats data frame
      if (exists("pitching_stats", envir = .GlobalEnv)) {
        ps <- get("pitching_stats", envir = .GlobalEnv)
        total_er <- sum(ps$ER, na.rm = TRUE)
        total_outs <- sum(ps$OutsRecorded, na.rm = TRUE)
        total_hits <- sum(ps$H, na.rm = TRUE)
        total_bb <- sum(ps$BB, na.rm = TRUE)
        ip <- total_outs / 3
        league_era <- if (ip > 0) 9 * total_er / ip else NA
        league_whip <- if (ip > 0) (total_hits + total_bb) / ip else NA
        ERA <- safe_era(total_er, ip)
        WHIP <- safe_whip(total_bb, total_hits, ip, sum(ps$HBP, na.rm = TRUE))
        K_per_9 <- safe_per_nine(sum(ps$K, na.rm = TRUE), ip)
        BB_per_9 <- safe_per_nine(total_bb, ip)
        HR_per_9 <- safe_per_nine(sum(ps$HR, na.rm = TRUE), ip)
        K_pct <- safe_divide(sum(ps$K, na.rm = TRUE), sum(ps$BF, na.rm = TRUE))
        BB_pct <- safe_divide(total_bb, sum(ps$BF, na.rm = TRUE))
        data.frame(League_ERA = ERA, League_WHIP = WHIP)
      } else {
        data.frame(League_ERA = 4.50, League_WHIP = 1.30)
      }
    }, error = function(e) {
      cat("Error getting pitching averages:", e$message, "\n")
      return(data.frame(
        League_ERA = 4.50,
        League_WHIP = 1.30
      ))
    })
    
    if (!is.null(pitching_avgs) && is.data.frame(pitching_avgs) && nrow(pitching_avgs) > 0) {
      # Use safer conversion with explicit NA checking
      result$pitching$League_ERA <- ifelse(is.na(pitching_avgs$League_ERA[1]), 4.50, as.numeric(pitching_avgs$League_ERA[1]))
      result$pitching$League_WHIP <- ifelse(is.na(pitching_avgs$League_WHIP[1]), 1.30, as.numeric(pitching_avgs$League_WHIP[1]))
    }
    
  }, error = function(e) {
    cat("Error getting league averages:", e$message, "\n")
    # Keep default values
  })
  
  return(result)
}