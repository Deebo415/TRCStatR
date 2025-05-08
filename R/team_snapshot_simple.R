# Improved Team Snapshot Module
# Uses dependency injection instead of global variables
# Enhanced with team logos, team-themed styling, team records, and team leaders sections

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)


# Declare global variables to avoid lint warnings
utils::globalVariables(c(
  "TeamID", "TeamName", "PlayerID", "FirstName", "LastInitial",
  "HomeTeamID", "AwayTeamID", "IsComplete", "HomeScore", "AwayScore",
  "X1B", "X2B", "X3B", "HR", "PA", "AB", "BB", "HBP", "SF", "H",
  "Pitch_ER", "IP", "Pitch_BB", "Pitch_H", "Pitch_HBP", "Pitch_K", "ERA"
))

#' Format average statistic for display
#' 
#' @param value Numeric value to format
#' @param stat_type Type of statistic (defaults to "AVG")
#' @return Formatted string
format_avg_stat <- function(value, stat_type = "AVG") {
  # Check if format_stat exists in object_engine.R
  if (exists("format_stat", envir = .GlobalEnv)) {
    format_fn <- get("format_stat", envir = .GlobalEnv)
    return(format_fn(value, stat_type))
  }
  
  # Fallback implementation
  if (is.null(value) || is.na(value)) {
    return(".000")
  }
  
  # Handle values of 1.000 or greater properly
  if (value >= 1) {
    # Format as 1.000 or higher for values >= 1
    return(sprintf("%d.%03d", floor(value), round((value - floor(value)) * 1000)))
  } else if (value <= 0) {
    return(".000")
  }
  
  # Standard formatting for values between 0 and 1
  sprintf(".%03d", round(value * 1000))
}

#' Generate a team snapshot
#'
#' @param team_id The ID of the team to snapshot
#' @param teams_df Data frame of team information
#' @param players_df Data frame of player information
#' @param get_player_stats Function to get player statistics
#' @param team_colors_list List of team colors
#' @param games_df Data frame of game information
#' @return An HTML UI element displaying the team snapshot
generate_team_snapshot <- function(team_id, teams_df, players_df, get_player_stats, 
                                   team_colors_list = NULL, games_df = NULL) {
  # Log the function call
  cat("Generating team snapshot for team ID:", team_id, "\n")
  
  # Get team info - used throughout the function
  team_info <- teams_df[teams_df$TeamID == as.integer(team_id), ]
  
  if (nrow(team_info) == 0) {
    return(div(
      class = "alert alert-danger",
      h3("Team Not Found"),
      p(paste("No team found with ID:", team_id))
    ))
  }
  
  team_name <- as.character(team_info$TeamName[1])
  cat("Team info retrieved:", team_name, "\n")
  
  # Get team logo path - in Shiny, files in www directory are served directly without the www/ prefix
  # Special case for Red Sox
  if (tolower(team_name) == "red sox") {
    logo_path <- "red_sox.png"
  } else {
    logo_path <- paste0(gsub(" ", "_", tolower(team_name)), ".png")
  }
  cat("Logo path:", logo_path, "\n")
  
  # Get team colors - used throughout the function for UI styling
  team_colors <- list(
    primary = "#1E88E5",   # Default blue
    secondary = "#0D47A1", # Darker blue
    tertiary = "#FFFFFF"   # White
  )
  if (!is.null(team_colors_list) && team_name %in% names(team_colors_list)) {
    team_colors <- team_colors_list[[team_name]]
  }
  
  # Get players on this team - used for calculating team stats and displaying player info
  team_players <- players_df[players_df$TeamID == team_id, ]
  
  # Check for master tables first
  # This variable is used throughout the function to determine whether to calculate stats locally
  use_master_tables <- FALSE
  
  # Add logging for debugging
  cat("Checking for master tables for team ID:", team_id, "\n")
  
  # Try to get team stats from master tables
  tryCatch({
    if (exists("master_team_batting_stats", envir = .GlobalEnv) && 
        exists("master_team_pitching_stats", envir = .GlobalEnv)) {
      
      cat("Found master tables in global environment\n")
      
      master_team_batting <- get("master_team_batting_stats", envir = .GlobalEnv)
      master_team_pitching <- get("master_team_pitching_stats", envir = .GlobalEnv)
      
      cat("Master team batting table dimensions: ", paste(dim(master_team_batting), collapse="x"), "\n")
      cat("Master team pitching table dimensions: ", paste(dim(master_team_pitching), collapse="x"), "\n")
      
      # Check if this team exists in the master tables
      team_in_batting <- any(master_team_batting$TeamID == as.integer(team_id))
      team_in_pitching <- any(master_team_pitching$TeamID == as.integer(team_id))
      
      cat("Team found in batting table: ", team_in_batting, "\n")
      cat("Team found in pitching table: ", team_in_pitching, "\n")
      
      if (team_in_batting && team_in_pitching) {
        # Get this team's stats from master tables
        team_batting_master <- master_team_batting[master_team_batting$TeamID == as.integer(team_id), ]
        team_pitching_master <- master_team_pitching[master_team_pitching$TeamID == as.integer(team_id), ]
        
        cat("Team batting rows found: ", nrow(team_batting_master), "\n")
        cat("Team pitching rows found: ", nrow(team_pitching_master), "\n")
        
        if (nrow(team_batting_master) > 0 && nrow(team_pitching_master) > 0) {
          # Use master tables
          use_master_tables <- TRUE
          cat("Using master tables for team stats\n")
          
          # Extract stats from master tables
          team_batting_stats <- list(
            AVG = team_batting_master$AVG[1],
            OBP = team_batting_master$OBP[1],
            SLG = team_batting_master$SLG[1],
            OPS = team_batting_master$OPS[1],
            HR = team_batting_master$HR[1],
            RBI = team_batting_master$RBI[1]
          )
          
          team_pitching_stats <- list(
            ERA = team_pitching_master$ERA[1],
            WHIP = team_pitching_master$WHIP[1],
            K_pct = ifelse(is.null(team_pitching_master$K_pct), NA, team_pitching_master$K_pct[1]),
            BB_pct = ifelse(is.null(team_pitching_master$BB_pct), NA, team_pitching_master$BB_pct[1])
          )
          
          # Log the values for debugging
          cat("Team batting stats from master tables:\n")
          cat("  AVG: ", team_batting_stats$AVG, "\n")
          cat("  OBP: ", team_batting_stats$OBP, "\n")
          cat("  SLG: ", team_batting_stats$SLG, "\n")
          cat("  OPS: ", team_batting_stats$OPS, "\n")
          
          cat("Team pitching stats from master tables:\n")
          cat("  ERA: ", team_pitching_stats$ERA, "\n")
          cat("  WHIP: ", team_pitching_stats$WHIP, "\n")
        } else {
          cat("Team found in master tables but no rows returned\n")
        }
      } else {
        cat("Team not found in one or both master tables\n")
      }
    } else {
      cat("Master tables not found in global environment\n")
    }
  }, error = function(e) {
    cat("Error accessing master tables: ", e$message, "\n")
  })
  
  # If master tables aren't available or don't have this team, create local stats
  if (!use_master_tables) {
    # Initialize with default values
    team_batting_stats <- list(
      AVG = NA, OBP = NA, SLG = NA, OPS = NA,
      HR = NA, RBI = NA
    )
    
    team_pitching_stats <- list(
      ERA = NA, WHIP = NA, K_pct = NA, BB_pct = NA
    )
  }
  
  # Calculate team batting and pitching stats only if we're not using master tables
  if (!use_master_tables && nrow(team_players) > 0) {
    # Aggregates for batting stats
    total_hits <- 0
    total_ab <- 0
    total_pa <- 0
    total_bb <- 0
    total_hbp <- 0
    total_sf <- 0
    total_singles <- 0
    total_doubles <- 0
    total_triples <- 0
    total_hr <- 0
    total_tb <- 0
    
    # Aggregates for pitching stats
    total_er <- 0
    total_ip <- 0
    total_bb_allowed <- 0
    total_h_allowed <- 0
    total_batters_hit <- 0
    total_k <- 0
    
    # Process each player
    for (i in seq_len(nrow(team_players))) {
      player_id <- team_players$PlayerID[i]
      
      # Get this player's stats
      player_stats <- get_player_stats(player_id)
      
      if (!is.null(player_stats)) {
        # Add to batting stats
        if (!is.null(player_stats$AB)) total_ab <- total_ab + player_stats$AB
        if (!is.null(player_stats$H)) total_hits <- total_hits + player_stats$H
        if (!is.null(player_stats$PA)) total_pa <- total_pa + player_stats$PA
        if (!is.null(player_stats$BB)) total_bb <- total_bb + player_stats$BB
        if (!is.null(player_stats$HBP)) total_hbp <- total_hbp + player_stats$HBP
        if (!is.null(player_stats$SF)) total_sf <- total_sf + player_stats$SF
        if (!is.null(player_stats$X1B)) total_singles <- total_singles + player_stats$X1B
        if (!is.null(player_stats$X2B)) total_doubles <- total_doubles + player_stats$X2B
        if (!is.null(player_stats$X3B)) total_triples <- total_triples + player_stats$X3B
        if (!is.null(player_stats$HR)) total_hr <- total_hr + player_stats$HR
        
        # Add to pitching stats
        if (!is.null(player_stats$Pitch_ER)) total_er <- total_er + player_stats$Pitch_ER
        if (!is.null(player_stats$IP)) total_ip <- total_ip + player_stats$IP
        if (!is.null(player_stats$Pitch_BB)) total_bb_allowed <- total_bb_allowed + player_stats$Pitch_BB
        if (!is.null(player_stats$Pitch_H)) total_h_allowed <- total_h_allowed + player_stats$Pitch_H
        if (!is.null(player_stats$Pitch_HBP)) total_batters_hit <- total_batters_hit + player_stats$Pitch_HBP
        if (!is.null(player_stats$Pitch_K)) total_k <- total_k + player_stats$Pitch_K
      }
    }
    
    # Calculate total bases
    total_tb <- total_singles + 2*total_doubles + 3*total_triples + 4*total_hr
    
    # Calculate team batting stats using safe functions if available
    safe_batting_avg <- if(exists("safe_batting_avg", envir = .GlobalEnv)) {
      get("safe_batting_avg", envir = .GlobalEnv)
    } else {
      function(h, ab) { if(ab > 0) h/ab else 0 }
    }
    
    safe_slg <- if(exists("safe_slg", envir = .GlobalEnv)) {
      get("safe_slg", envir = .GlobalEnv)
    } else {
      function(tb, ab) { if(ab > 0) tb/ab else 0 }
    }
    
    safe_obp <- if(exists("safe_obp", envir = .GlobalEnv)) {
      get("safe_obp", envir = .GlobalEnv)
    } else {
      function(h, bb, hbp, ab, sf) {
        if((ab + bb + hbp + sf) > 0) (h + bb + hbp)/(ab + bb + hbp + sf) else 0
      }
    }
    
    team_batting_stats$AVG <- safe_batting_avg(total_hits, total_ab)
    team_batting_stats$SLG <- safe_slg(total_tb, total_ab)
    team_batting_stats$OBP <- safe_obp(total_hits, total_bb, total_hbp, total_ab, total_sf)
    team_batting_stats$OPS <- team_batting_stats$OBP + team_batting_stats$SLG
    team_batting_stats$HR <- total_hr
    team_batting_stats$RBI <- sum(sapply(team_players$PlayerID, function(pid) {
      player_stats <- get_player_stats(pid)
      if (!is.null(player_stats) && !is.null(player_stats$RBI)) {
        return(player_stats$RBI)
      }
      return(0)
    }))
    
    # Calculate team pitching stats using safe functions if available
    safe_era <- if(exists("safe_era", envir = .GlobalEnv)) {
      get("safe_era", envir = .GlobalEnv)
    } else {
      function(er, ip) { if(ip > 0) 9 * er / ip else 0 }
    }
    
    safe_whip <- if(exists("safe_whip", envir = .GlobalEnv)) {
      get("safe_whip", envir = .GlobalEnv)
    } else {
      function(bb, h, hbp, ip) { if(ip > 0) (bb + h + hbp) / ip else 0 }
    }
    
    team_pitching_stats$ERA <- safe_era(total_er, total_ip)
    team_pitching_stats$WHIP <- safe_whip(total_bb_allowed, total_h_allowed, total_batters_hit, total_ip)
    
    # Calculate K% and BB% instead of K/9 and BB/9
    total_batters_faced <- total_ab + total_bb + total_hbp + total_sf
    team_pitching_stats$K_pct <- if(total_batters_faced > 0) total_k / total_batters_faced else 0
    team_pitching_stats$BB_pct <- if(total_batters_faced > 0) total_bb_allowed / total_batters_faced else 0
  }
  
  # Get team games
  team_games <- data.frame()
  if (!is.null(games_df)) {
    team_games <- games_df[
      (games_df$HomeTeamID == as.integer(team_id) | games_df$AwayTeamID == as.integer(team_id)) &
        games_df$IsComplete == 1,
    ]
  }
  
  # Get team record from master_team_standings if available
  wins <- 0
  losses <- 0
  ties <- 0
  pythag_pct <- 0  # Add a default value for pythag_pct
  
  if (exists("master_team_standings", envir = .GlobalEnv)) {
    standings <- get("master_team_standings", envir = .GlobalEnv)
    team_standing <- standings[standings$TeamID == as.integer(team_id), ]
    
    if (nrow(team_standing) > 0) {
      wins <- team_standing$W[1]
      losses <- team_standing$L[1]
      ties <- team_standing$T[1]
      pythag_pct <- team_standing$pythag_pct[1]
      cat("Found team in standings: ", team_name, " (", wins, "-", losses, "-", ties, ")\n", sep="")
    }
  }
  
  if (nrow(team_games) > 0) {
    for (i in seq_len(nrow(team_games))) {
      game <- team_games[i, ]
      home_score <- as.numeric(game$HomeScore)
      away_score <- as.numeric(game$AwayScore)
      
      if (game$HomeTeamID == as.integer(team_id)) {
        if (home_score > away_score) wins <- wins + 1
        else losses <- losses + 1
      } else {
        if (away_score > home_score) wins <- wins + 1
        else losses <- losses + 1
      }
    }
  }
  
  # Format stats for display with error handling
  tryCatch({
    batting_avg_display <- format_avg_stat(team_batting_stats$AVG)
    obp_display <- format_avg_stat(team_batting_stats$OBP)
    slg_display <- format_avg_stat(team_batting_stats$SLG)
    ops_display <- format_avg_stat(team_batting_stats$OPS)
    
    # Handle potential NA values in pitching stats
    era_value <- if(is.null(team_pitching_stats$ERA) || is.na(team_pitching_stats$ERA)) NA else team_pitching_stats$ERA
    whip_value <- if(is.null(team_pitching_stats$WHIP) || is.na(team_pitching_stats$WHIP)) NA else team_pitching_stats$WHIP
    k_pct_value <- if(is.null(team_pitching_stats$K_pct) || is.na(team_pitching_stats$K_pct)) NA else team_pitching_stats$K_pct
    bb_pct_value <- if(is.null(team_pitching_stats$BB_pct) || is.na(team_pitching_stats$BB_pct)) NA else team_pitching_stats$BB_pct
    
    era_display <- if(is.na(era_value)) "-.--" else sprintf("%.2f", era_value)
    whip_display <- if(is.na(whip_value)) "-.--" else sprintf("%.2f", whip_value)
    k_pct_display <- if(is.na(k_pct_value)) "-.-%" else sprintf("%.1f%%", k_pct_value * 100)
    bb_pct_display <- if(is.na(bb_pct_value)) "-.-%" else sprintf("%.1f%%", bb_pct_value * 100)
    
    # Log formatted values for debugging
    cat("Formatted batting stats for display:\n")
    cat("  AVG: ", batting_avg_display, "\n")
    cat("  OBP: ", obp_display, "\n")
    cat("  SLG: ", slg_display, "\n")
    cat("  OPS: ", ops_display, "\n")
    
    cat("Formatted pitching stats for display:\n")
    cat("  ERA: ", era_display, "\n")
    cat("  WHIP: ", whip_display, "\n")
  }, error = function(e) {
    cat("Error formatting stats for display: ", e$message, "\n")
    
    # Set default values if there's an error
    batting_avg_display <<- ".---"
    obp_display <<- ".---"
    slg_display <<- ".---"
    ops_display <<- ".---"
    
    era_display <<- "-.--"
    whip_display <<- "-.--"
    k_pct_display <<- "-.-%"
    bb_pct_display <<- "-.-%"
  })
  
  # Get league ranks for team stats
  team_ranks <- list(
    AVG = NA, OBP = NA, SLG = NA, OPS = NA,
    ERA = NA, WHIP = NA, K_pct = NA, BB_pct = NA
  )
  
  # If master_team_stats exists, get ranks
  if (exists("master_team_stats", envir = .GlobalEnv)) {
    team_stats <- get("master_team_stats", envir = .GlobalEnv)
    
    # Function to get rank (ascending = lower is better, like ERA)
    get_rank <- function(stat_name, team_value, ascending = FALSE) {
      if (stat_name %in% names(team_stats) && !is.na(team_value)) {
        # Sort teams by the stat
        if (ascending) {
          sorted_teams <- team_stats[order(team_stats[[stat_name]]), ]
        } else {
          sorted_teams <- team_stats[order(team_stats[[stat_name]], decreasing = TRUE), ]
        }
        
        # Find this team's rank
        rank <- which(sorted_teams$TeamID == as.integer(team_id))
        return(rank)
      }
      return(NA)
    }
    
    # Get ranks for batting stats (higher is better)
    team_ranks$AVG <- get_rank("AVG", team_batting_stats$AVG)
    team_ranks$OBP <- get_rank("OBP", team_batting_stats$OBP)
    team_ranks$SLG <- get_rank("SLG", team_batting_stats$SLG)
    team_ranks$OPS <- get_rank("OPS", team_batting_stats$OPS)
    
    # Get ranks for pitching stats (lower is better for ERA, WHIP, BB_pct)
    team_ranks$ERA <- get_rank("ERA", team_pitching_stats$ERA, TRUE)
    team_ranks$WHIP <- get_rank("WHIP", team_pitching_stats$WHIP, TRUE)
    team_ranks$K_pct <- get_rank("K_pct", team_pitching_stats$K_pct)
    team_ranks$BB_pct <- get_rank("BB_pct", team_pitching_stats$BB_pct, TRUE)
  }
  
  # Format rank display
  format_rank <- function(rank) {
    if (is.na(rank)) return("")
    
    # Add ordinal suffix
    suffix <- switch(
      as.character(rank %% 10),
      "1" = if (rank %% 100 == 11) "th" else "st",
      "2" = if (rank %% 100 == 12) "th" else "nd",
      "3" = if (rank %% 100 == 13) "th" else "rd",
      "th"
    )
    
    return(paste0(" (", rank, suffix, ")"))
  }
  
  # Create comprehensive team leaders sections
  # 1. Batting Leaders
  batting_categories <- c("AVG", "OBP", "SLG", "OPS", "SB")
  batting_leaders <- list()
  
  # 2. Pitching Leaders
  pitching_categories <- c("ERA", "WHIP", "Pitch_K", "K_pct", "BB_pct")
  pitching_leaders <- list()
  
  # Get master tables if available for more efficient lookups
  if (exists("master_batting_stats", envir = .GlobalEnv) && 
      exists("master_pitching_stats", envir = .GlobalEnv)) {
    
    master_batting <- get("master_batting_stats", envir = .GlobalEnv)
    master_pitching <- get("master_pitching_stats", envir = .GlobalEnv)
    
    # Filter for this team's players
    team_batting <- master_batting[master_batting$TeamID == as.integer(team_id), ]
    team_pitching <- master_pitching[master_pitching$TeamID == as.integer(team_id), ]
    
    # Process batting leaders
    for (category in batting_categories) {
      # Handle special cases
      if (category == "SB") {
        # Sort by SB, descending
        if ("SB" %in% names(team_batting) && nrow(team_batting) > 0) {
          sorted_players <- team_batting[order(team_batting$SB, decreasing = TRUE), ]
          top_players <- head(sorted_players, 3)
          
          batting_leaders[[category]] <- data.frame(
            Name = top_players$Player,
            Value = top_players$SB,
            stringsAsFactors = FALSE
          )
        }
      } else {
        # For AVG, OBP, SLG, OPS - sort descending
        if (category %in% names(team_batting) && nrow(team_batting) > 0) {
          sorted_players <- team_batting[order(team_batting[[category]], decreasing = TRUE), ]
          top_players <- head(sorted_players, 3)
          
          batting_leaders[[category]] <- data.frame(
            Name = top_players$Player,
            Value = top_players[[category]],
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    # Process pitching leaders
    for (category in pitching_categories) {
      if (category %in% names(team_pitching) && nrow(team_pitching) > 0) {
        # For ERA, WHIP, BB_pct - sort ascending (lower is better)
        if (category %in% c("ERA", "WHIP", "BB_pct")) {
          sorted_players <- team_pitching[order(team_pitching[[category]]), ]
        } else {
          # For K, K_pct - sort descending (higher is better)
          sorted_players <- team_pitching[order(team_pitching[[category]], decreasing = TRUE), ]
        }
        
        top_players <- head(sorted_players, 3)
        
        pitching_leaders[[category]] <- data.frame(
          Name = top_players$Player,
          Value = top_players[[category]],
          stringsAsFactors = FALSE
        )
      }
    }
  } else {
    # Fallback to the old method if master tables aren't available
    for (i in seq_len(nrow(team_players))) {
      player_id <- team_players$PlayerID[i]
      player_name <- paste(team_players$FirstName[i], team_players$LastInitial[i])
      
      # Get player stats
      player_stats <- get_player_stats(player_id)
      
      if (!is.null(player_stats)) {
        # Add to batting leaders
        for (category in batting_categories) {
          if (!is.null(player_stats[[category]])) {
            if (is.null(batting_leaders[[category]])) {
              batting_leaders[[category]] <- data.frame(
                Name = character(),
                Value = numeric(),
                stringsAsFactors = FALSE
              )
            }
            
            batting_leaders[[category]] <- rbind(
              batting_leaders[[category]],
              data.frame(Name = player_name, Value = player_stats[[category]], stringsAsFactors = FALSE)
            )
          }
        }
        
        # Add to pitching leaders
        for (category in pitching_categories) {
          if (!is.null(player_stats[[category]])) {
            if (is.null(pitching_leaders[[category]])) {
              pitching_leaders[[category]] <- data.frame(
                Name = character(),
                Value = numeric(),
                stringsAsFactors = FALSE
              )
            }
            
            pitching_leaders[[category]] <- rbind(
              pitching_leaders[[category]],
              data.frame(Name = player_name, Value = player_stats[[category]], stringsAsFactors = FALSE)
            )
          }
        }
      }
    }
    
    # Sort and limit to top 3 for each category
    for (category in batting_categories) {
      if (!is.null(batting_leaders[[category]]) && nrow(batting_leaders[[category]]) > 0) {
        batting_leaders[[category]] <- batting_leaders[[category]][order(batting_leaders[[category]]$Value, decreasing = TRUE), ]
        batting_leaders[[category]] <- head(batting_leaders[[category]], 3)
      }
    }
    
    for (category in pitching_categories) {
      if (!is.null(pitching_leaders[[category]]) && nrow(pitching_leaders[[category]]) > 0) {
        # For ERA, WHIP, BB_pct - sort ascending (lower is better)
        decreasing <- !(category %in% c("ERA", "WHIP", "BB_pct"))
        pitching_leaders[[category]] <- pitching_leaders[[category]][order(pitching_leaders[[category]]$Value, decreasing = decreasing), ]
        pitching_leaders[[category]] <- head(pitching_leaders[[category]], 3)
      }
    }
  }
  
  # Build the snapshot UI with team-themed styling like Player Card
  snapshot_ui <- div(
    class = "team-snapshot",
    style = paste0(
      "border: 2px solid ", team_colors$primary, "; ",
      "border-radius: 8px; padding: 15px; ",
      "margin-bottom: 20px; background-color: ", team_colors$tertiary, ";"
    ),
    
    # Team header with logo and record
    div(
      class = "team-header",
      style = paste0(
        "display: flex; align-items: center; ",
        "background-color: ", team_colors$primary, "; ",
        "padding: 15px; border-radius: 5px; margin-bottom: 15px; ",
        "box-shadow: 0 3px 6px rgba(0,0,0,0.16);"
      ),
      div(
        style = "margin-right: 20px;",
        # Use the proper team logo path
        tags$img(
          src = logo_path, 
          height = "100px", 
          alt = paste(team_name, "logo"), 
          style = "filter: drop-shadow(2px 2px 3px rgba(0,0,0,0.3));",
          # Add onerror handler to show a default image if the team logo fails to load
          onerror = "this.onerror=null; this.src='default_team.png';"
        )
      ),
      div(
        h2(team_name, 
           style = paste0(
             "margin: 0; font-weight: bold; color: white; ",
             "font-family: 'Freshman', sans-serif; ",
             "font-size: 2.7em; letter-spacing: 1.5px; ",
             "text-shadow: -1px -1px 0 ", team_colors$secondary, ", ",
             "1px -1px 0 ", team_colors$secondary, ", ",
             "-1px 1px 0 ", team_colors$secondary, ", ",
             "1px 1px 0 ", team_colors$secondary, ", ",
             "2px 2px 3px rgba(0,0,0,0.5);"
           )),
        h3(paste("Record:", wins, "-", losses, if(ties > 0) paste0("-", ties) else ""), 
           style = paste0(
             "margin: 5px 0 0 0; color: ", team_colors$tertiary, "; ",
             "font-family: 'Freshman', sans-serif; ",
             "text-shadow: -1px -1px 0 ", team_colors$secondary, ", ",
             "1px -1px 0 ", team_colors$secondary, ", ",
             "-1px 1px 0 ", team_colors$secondary, ", ",
             "1px 1px 0 ", team_colors$secondary, ", ",
             "2px 2px 3px rgba(0,0,0,0.5);"
           )),
        div(
          style = "display: flex; align-items: center; margin: 5px 0 0 0;",
          h3("Pythagorean Win % : ", 
             style = paste0(
               "margin: 0; color: ", team_colors$tertiary, "; ",
               "font-family: 'Freshman', sans-serif; ",
               "text-shadow: -1px -1px 0 ", team_colors$secondary, ", ",
               "1px -1px 0 ", team_colors$secondary, ", ",
               "-1px 1px 0 ", team_colors$secondary, ", ",
               "1px 1px 0 ", team_colors$secondary, ", ",
               "2px 2px 3px rgba(0,0,0,0.5);"
             )),
          h3(paste0(" "),round(pythag_pct,3), 
             style = paste0(
               "margin: 0 5px 0 0; color: ", team_colors$tertiary, "; ",
               "font-family: 'Freshman', sans-serif; ",
               "text-shadow: -1px -1px 0 ", team_colors$secondary, ", ",
               "1px -1px 0 ", team_colors$secondary, ", ",
               "-1px 1px 0 ", team_colors$secondary, ", ",
               "1px 1px 0 ", team_colors$secondary, ", ",
               "2px 2px 3px rgba(0,0,0,0.5);"
             )),
          span(
            class = "stat-tooltip stat-help-icon",
            title = "Pythagorean win percentage is a formula used to estimate what a team's winning percentage SHOULD BE based on runs scored and runs allowed. Developed by baseball statistician Bill James in the 1980s.",
            icon("lightbulb")
          )
        )
      )
    ),
    
    # Team stats section
    div(
      class = "stats-section",
      div(
        class = "section-header",
        style = paste0(
          "background-color: ", team_colors$secondary, "; color: white; ",
          "padding: 8px 15px; border-radius: 5px; margin: 20px 0 15px 0; ",
          "font-family: 'Freshman', sans-serif; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
        ),
        h3("Team Stats", style = "margin: 0; color: white; 
                                  text-align: center; font-family: 'Freshman', sans-serif;
                                  text-shadow: -1px -1px 0 #000000, 
                                  1px -1px 0 #000000, 
                                  -1px 1px 0 #000000, 
                                  1px 1px 0 #000000, 
                                  2px 2px 3px rgba(0,0,0,0.5);")
      ),
      
      # Two-column layout
      div(
        style = "display: flex; flex-wrap: wrap;",
        
        # Batting stats
        div(
          style = "flex: 1; min-width: 200px; margin-right: 10px;",
          h4("Batting", style = "border-bottom: 1px solid #ddd; 
             padding-bottom: 5px;
             font-family: 'Freshman', sans-serif;"),
          div(
            style = "margin-bottom: 5px; display: flex; justify-content: space-between;",
            tags$span("AVG: ", style = "font-family: 'Freshman', sans-serif;"),
            tags$span(
              HTML(paste0(batting_avg_display, 
                          "<span style='color: #666 !important; font-size: 0.9em !important;'>", 
                          format_rank(team_ranks$AVG), "</span>")),
              style = "font-weight: bold !important; font-size: 1.1em !important; display: inline-block !important;"
            )
          ),
          div(
            style = "margin-bottom: 5px; display: flex; justify-content: space-between;",
            tags$span("OBP: ", style = "font-family: 'Freshman', sans-serif;"),
            tags$span(
              HTML(paste0(obp_display, 
                          "<span style='color: #666 !important; font-size: 0.9em !important;'>", 
                          format_rank(team_ranks$OBP), "</span>")),
              style = "font-weight: bold !important; font-size: 1.1em !important; display: inline-block !important;"
            )
          ),
          div(
            style = "margin-bottom: 5px; display: flex; justify-content: space-between;",
            tags$span("SLG: ", style = "font-family: 'Freshman', sans-serif;"),
            tags$span(
              HTML(paste0(slg_display, 
                          "<span style='color: #666 !important; font-size: 0.9em !important;'>", 
                          format_rank(team_ranks$SLG), "</span>")),
              style = "font-weight: bold !important; font-size: 1.1em !important; display: inline-block !important;"
            )
          ),
          div(
            style = "margin-bottom: 5px; display: flex; justify-content: space-between;",
            tags$span("OPS: ", style = "font-family: 'Freshman', sans-serif;"),
            tags$span(
              HTML(paste0(ops_display, 
                          "<span style='color: #666 !important; font-size: 0.9em !important;'>", 
                          format_rank(team_ranks$OPS), "</span>")),
              style = "font-weight: bold !important; font-size: 1.1em !important; display: inline-block !important;"
            )
          )
        ),
        
        # Pitching stats
        div(
          style = "flex: 1; min-width: 200px;",
          h4("Pitching", style = "border-bottom: 1px solid #ddd; 
             padding-bottom: 5px;
             font-family: 'Freshman', sans-serif;"),
          div(
            style = "margin-bottom: 5px; display: flex; justify-content: space-between;",
            tags$span("ERA: ", style = "font-family: 'Freshman', sans-serif;"),
            tags$span(
              HTML(paste0(era_display, 
                          "<span style='color: #666 !important; font-size: 0.9em !important;'>", 
                          format_rank(team_ranks$ERA), "</span>")),
              style = "font-weight: bold !important; font-size: 1.1em !important; display: inline-block !important;"
            )
          ),
          div(
            style = "margin-bottom: 5px; display: flex; justify-content: space-between;",
            tags$span("WHIP: ", style = "font-family: 'Freshman', sans-serif;"),
            tags$span(
              HTML(paste0(whip_display, 
                          "<span style='color: #666 !important; font-size: 0.9em !important;'>", 
                          format_rank(team_ranks$WHIP), "</span>")),
              style = "font-weight: bold !important; font-size: 1.1em !important; display: inline-block !important;"
            )
          ),
          div(
            style = "margin-bottom: 5px; display: flex; justify-content: space-between;",
            tags$span("K%: ", style = "font-family: 'Freshman', sans-serif;"),
            tags$span(
              HTML(paste0(k_pct_display, 
                          "<span style='color: #666 !important; font-size: 0.9em !important;'>", 
                          format_rank(team_ranks$K_pct), "</span>")),
              style = "font-weight: bold !important; font-size: 1.1em !important; display: inline-block !important;"
            )
          ),
          div(
            style = "margin-bottom: 5px; display: flex; justify-content: space-between;",
            tags$span("BB%: ", style = "font-family: 'Freshman', sans-serif;"),
            tags$span(
              HTML(paste0(bb_pct_display, 
                          "<span style='color: #666 !important; font-size: 0.9em !important;'>", 
                          format_rank(team_ranks$BB_pct), "</span>")),
              style = "font-weight: bold !important; font-size: 1.1em !important; display: inline-block !important;"
            )
          )
        )
      )
    ),
    
    # Team leaders section with batting and pitching categories
    div(
      class = "leaders-section",
      div(
        class = "section-header",
        style = paste0(
          "background-color: ", team_colors$secondary, "; color: white; ",
          "padding: 8px 15px; border-radius: 5px; margin: 20px 0 15px 0; ",
          "font-family: 'Freshman', sans-serif; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
        ),
        h3("Team Leaders", style = "margin: 0; color: white; 
                                    text-align: center; 
                                    font-family: 'Freshman', sans-serif;
                                    text-shadow: -1px -1px 0 #000000, 
                                    1px -1px 0 #000000, 
                                    -1px 1px 0 #000000, 
                                    1px 1px 0 #000000, 
                                    2px 2px 3px rgba(0,0,0,0.5);")
      ),
      
      # Batting Leaders Section
      div(
        style = paste0(
          "background-color: ", team_colors$primary, "; ",
          "border: 1px solid ", team_colors$secondary, "; ",
          "border-radius: 5px; padding: 15px; margin-bottom: 20px;"
        ),
        h4("Batting Leaders", 
           style = paste0(
             "color: ", team_colors$tertiary, "; ",
             "border-bottom: 2px solid ", team_colors$secondary, "; ",
             "padding-bottom: 8px; margin-top: 0; ",
             "font-family: 'Freshman', sans-serif; ",
             "text-shadow: -1px -1px 0 ", team_colors$secondary, ", ",
             "1px -1px 0 ", team_colors$secondary, ", ",
             "-1px 1px 0 ", team_colors$secondary, ", ",
             "1px 1px 0 ", team_colors$secondary, ", ",
             "2px 2px 3px rgba(0,0,0,0.5);"
           )),
        
        # Batting categories in a flex layout
        div(
          style = "display: flex; flex-wrap: wrap; 
          gap: 20px; justify-content: space-between;",
          
          # Generate a box for each batting category
          lapply(batting_categories, function(category) {
            # Format the display name
            display_name <- switch(category,
                                   "AVG" = "Batting Average",
                                   "OBP" = "On-Base %",
                                   "SLG" = "Slugging %",
                                   "OPS" = "OPS",
                                   "SB" = "Stolen Bases",
                                   category)
            
            # Format function based on category
            format_fn <- switch(category,
                                "AVG" = function(x) format_avg_stat(x),
                                "OBP" = function(x) format_avg_stat(x),
                                "SLG" = function(x) format_avg_stat(x),
                                "OPS" = function(x) format_avg_stat(x),
                                "SB" = function(x) as.character(x),
                                function(x) as.character(x))
            
            div(
              style = paste0(
                "flex: 1; min-width: 150px; ",
                "background-color: white; ",
                "border-radius: 5px; ",
                "padding: 10px; ",
                "box-shadow: 0 1px 3px rgba(0,0,0,0.1);"
              ),
              h5(display_name, 
                 style = paste0(
                   "color: ", team_colors$primary, "; ",
                   "border-bottom: 1px solid ", team_colors$secondary, "; ",
                   "padding-bottom: 5px; margin-top: 0; font-size: 1.1em;
                   font-family: 'Freshman', sans-serif;"
                 )),
              if (!is.null(batting_leaders[[category]]) && nrow(batting_leaders[[category]]) > 0) {
                tagList(
                  lapply(seq_len(nrow(batting_leaders[[category]])), function(i) {
                    div(
                      style = "margin-bottom: 5px; display: flex; justify-content: space-between;",
                      tags$span(batting_leaders[[category]]$Name[i], style = "font-weight: bold;"),
                      tags$span(format_fn(batting_leaders[[category]]$Value[i]))
                    )
                  })
                )
              } else {
                div("No data available", style = "color: #888; font-style: italic; text-align: center;")
              }
            )
          })
        )
      ),
      
      # Pitching Leaders Section
      div(
        style = paste0(
          "background-color: ", team_colors$primary, "; ",
          "border: 1px solid ", team_colors$secondary, "; ",
          "border-radius: 5px; padding: 15px;"
        ),
        h4("Pitching Leaders", 
           style = paste0(
             "color: ", team_colors$tertiary, "; ",
             "border-bottom: 2px solid ", team_colors$secondary, "; ",
             "padding-bottom: 8px; margin-top: 0; ",
             "font-family: 'Freshman', sans-serif; ",
             "text-shadow: -1px -1px 0 ", team_colors$secondary, ", ",
             "1px -1px 0 ", team_colors$secondary, ", ",
             "-1px 1px 0 ", team_colors$secondary, ", ",
             "1px 1px 0 ", team_colors$secondary, ", ",
             "2px 2px 3px rgba(0,0,0,0.5);"
           )),
        
        # Pitching categories in a flex layout
        div(
          style = "display: flex; flex-wrap: wrap; 
          gap: 20px; justify-content: space-between;",
          
          # Generate a box for each pitching category
          lapply(pitching_categories, function(category) {
            # Format the display name
            display_name <- switch(category,
                                   "ERA" = "ERA",
                                   "WHIP" = "WHIP",
                                   "Pitch_K" = "Strikeouts",
                                   "K_pct" = "Strikeout %",
                                   "BB_pct" = "Walk %",
                                   category)
            
            # Format function based on category
            format_fn <- switch(category,
                                "ERA" = function(x) sprintf("%.2f", x),
                                "WHIP" = function(x) sprintf("%.2f", x),
                                "Pitch_K" = function(x) as.character(round(x)),
                                "K_pct" = function(x) sprintf("%.1f%%", x * 100),
                                "BB_pct" = function(x) sprintf("%.1f%%", x * 100),
                                function(x) as.character(x))
            
            div(
              style = paste0(
                "flex: 1; min-width: 150px; ",
                "background-color: white; ",
                "border-radius: 5px; ",
                "padding: 10px; ",
                "box-shadow: 0 1px 3px rgba(0,0,0,0.1);"
              ),
              h5(display_name, 
                 style = paste0(
                   "color: ", team_colors$primary, "; ",
                   "border-bottom: 1px solid ", team_colors$secondary, "; ",
                   "padding-bottom: 5px; margin-top: 0; font-size: 1.1em;
                   font-family: 'Freshman', sans-serif;"
                 )),
              if (!is.null(pitching_leaders[[category]]) && nrow(pitching_leaders[[category]]) > 0) {
                tagList(
                  lapply(seq_len(nrow(pitching_leaders[[category]])), function(i) {
                    div(
                      style = "margin-bottom: 5px; display: flex; justify-content: space-between;",
                      tags$span(pitching_leaders[[category]]$Name[i], style = "font-weight: bold;"),
                      tags$span(format_fn(pitching_leaders[[category]]$Value[i]))
                    )
                  })
                )
              } else {
                div("No data available", style = "color: #888; font-style: italic; text-align: center;")
              }
            )
          })
        )
      )
    )
  )
  
  return(snapshot_ui)
}

# UI Module for Team Snapshot
team_snapshot_ui_simple <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             div(style = "font-size: 50px; font-family: 'Freshman', sans-serif;"),
             box(
               title = "Team Snapshot",
               status = "primary",
               p("Generate a comprehensive snapshot of team performance with detailed statistics."),
               p("Select a team and click 'Generate Team Snapshot' to view team stats and metrics."),
               solidHeader = TRUE,
               width = 12,
               
               # Team selection
               selectInput(
                 ns("team_select"),
                 "Select Team:",
                 choices = NULL,
                 width = "40%"
               ),
               
               # Generate button
               actionButton(
                 ns("generate_snapshot"),
                 "Generate Team Snapshot",
                 icon = icon("chart-bar"),
                 class = "btn-primary",
                 width = "30%"
               ),
               
               # Output area
               div(
                 style = "margin-top: 20px;",
                 uiOutput(ns("snapshot_output"))
               )
             )
      )
    )
  )
}

# Server function for Team Snapshot
team_snapshot_server_simple <- function(id, get_teams, get_players, get_player_stats, get_team_colors, get_games) {
  moduleServer(id, function(input, output, session) {
    # Initialize reactive values
    values <- reactiveValues(
      current_team_id = NULL
    )
    
    # Load teams on initialization
    observe({
      teams_data <- get_teams()
      
      if (is.data.frame(teams_data) && nrow(teams_data) > 0) {
        team_choices <- setNames(teams_data$TeamID, teams_data$TeamName)
        updateSelectInput(session, "team_select", choices = c("Select a team" = "", team_choices))
      }
    })
    
    # Generate snapshot when button is clicked
    observeEvent(input$generate_snapshot, {
      req(input$team_select != "")
      
      # Convert team_id to integer
      team_id <- as.integer(input$team_select)
      
      # Get required data
      teams_df <- get_teams()
      players_df <- get_players()
      team_colors_list <- get_team_colors()
      games_df <- get_games()
      
      # Use the generate_team_snapshot function
      output$snapshot_output <- renderUI({
        withProgress(message = 'Generating team snapshot...', value = 0, {
          incProgress(0.3, detail = "Loading team data...")
          snapshot <- generate_team_snapshot(
            team_id = team_id,
            teams_df = teams_df,
            players_df = players_df,
            get_player_stats = get_player_stats,
            team_colors_list = team_colors_list,
            games_df = games_df
          )
          incProgress(0.7)
          snapshot
        })
      })
      
      # Store the current team ID
      values$current_team_id <- team_id
    })
  })
}