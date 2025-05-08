# Fixed Leaderboard Processors Module for TRCStatR
# This version removes any reactive code that might cause errors

# Helper function to join player and team names
join_player_team_names <- function(stats_data, players, teams) {
  if (is.null(stats_data) || nrow(stats_data) == 0) {
    return(data.frame())
  }
  
  # Join with player names
  if (!is.null(players) && nrow(players) > 0 && "PlayerID" %in% names(stats_data)) {
    stats_data <- merge(stats_data, players[, c("PlayerID", "FirstName", "LastName")], by = "PlayerID", all.x = TRUE)
    stats_data$PlayerName <- paste(stats_data$FirstName, stats_data$LastName)
  } else {
    stats_data$PlayerName <- "Unknown Player"
  }
  
  # Join with team names
  if (!is.null(teams) && nrow(teams) > 0 && "TeamID" %in% names(stats_data)) {
    stats_data <- merge(stats_data, teams[, c("TeamID", "TeamName")], by = "TeamID", all.x = TRUE)
  } else {
    stats_data$TeamName <- "Unknown Team"
  }
  
  return(stats_data)
}

# Process Total WAR (combined batting and pitching)
process_total_war <- function(players, teams, batting_data, pitching_data, league_avgs, count) {
  # Create empty dataframes if data is missing
  if (is.null(batting_data)) {
    batting_data <- data.frame(PlayerID=numeric(0), `1B`=numeric(0), `2B`=numeric(0), 
                              `3B`=numeric(0), HR=numeric(0), BB=numeric(0), PA=numeric(0),
                              SB=numeric(0))
  }
  if (is.null(pitching_data)) {
    pitching_data <- data.frame(PlayerID=numeric(0), OutsRecorded=numeric(0), ER=numeric(0), 
                               K=numeric(0), BB=numeric(0), HR=numeric(0))
  }
  
  # Calculate batting WAR for each player
  batting_war <- data.frame(PlayerID = numeric(0), sbWAR = numeric(0))
  if (nrow(batting_data) > 0) {
    # Calculate batting stats for each player
    for (i in 1:nrow(batting_data)) {
      player_id <- batting_data$PlayerID[i]
      player_stats <- batting_data[i, ]
      
      # Calculate WAR
      war_value <- calculate_bWAR(player_stats, league_avgs)
      
      # Add to results
      batting_war <- rbind(batting_war, data.frame(PlayerID = player_id, sbWAR = war_value))
    }
  }
  
  # Calculate pitching WAR for each player
  pitching_war <- data.frame(PlayerID = numeric(0), spWAR = numeric(0))
  if (nrow(pitching_data) > 0) {
    # Calculate pitching stats for each player
    for (i in 1:nrow(pitching_data)) {
      player_id <- pitching_data$PlayerID[i]
      player_stats <- pitching_data[i, ]
      
      # Calculate WAR
      war_value <- calculate_pWAR(player_stats, league_avgs)
      
      # Add to results
      pitching_war <- rbind(pitching_war, data.frame(PlayerID = player_id, spWAR = war_value))
    }
  }
  
  # Combine batting and pitching WAR
  all_players <- unique(c(batting_war$PlayerID, pitching_war$PlayerID))
  total_war <- data.frame(PlayerID = all_players, TotalWAR = 0)
  
  for (i in 1:nrow(total_war)) {
    player_id <- total_war$PlayerID[i]
    
    # Get batting WAR
    b_war <- 0
    if (player_id %in% batting_war$PlayerID) {
      b_war <- batting_war$sbWAR[batting_war$PlayerID == player_id]
    }
    
    # Get pitching WAR
    p_war <- 0
    if (player_id %in% pitching_war$PlayerID) {
      p_war <- pitching_war$spWAR[pitching_war$PlayerID == player_id]
    }
    
    # Calculate total WAR
    total_war$TotalWAR[i] <- b_war + p_war
  }
  
  # Sort by total WAR and limit to count
  total_war <- total_war[order(-total_war$TotalWAR), ]
  if (nrow(total_war) > count) {
    total_war <- total_war[1:count, ]
  }
  
  # Join with player and team names
  player_info <- players[, c("PlayerID", "FirstName", "LastName", "TeamID")]
  total_war <- merge(total_war, player_info, by = "PlayerID")
  total_war <- merge(total_war, teams[, c("TeamID", "TeamName")], by = "TeamID")
  
  # Format the result
  result <- data.frame(
    PlayerName = paste(total_war$FirstName, total_war$LastName),
    TeamName = total_war$TeamName,
    "Season WAR" = round(total_war$TotalWAR, 2)
  )
  
  return(result)
}

# Process Batting WAR
process_batting_war <- function(players, teams, batting_data, league_avgs, count) {
  if (is.null(batting_data) || nrow(batting_data) == 0) {
    return(data.frame())
  }
  
  # Calculate batting WAR for each player
  batting_war <- data.frame(PlayerID = numeric(0), sbWAR = numeric(0))
  
  for (i in 1:nrow(batting_data)) {
    player_id <- batting_data$PlayerID[i]
    player_stats <- batting_data[i, ]
    
    # Calculate WAR
    war_value <- calculate_bWAR(player_stats, league_avgs)
    
    # Add to results
    batting_war <- rbind(batting_war, data.frame(PlayerID = player_id, sbWAR = war_value))
  }
  
  # Sort by WAR and limit to count
  batting_war <- batting_war[order(-batting_war$sbWAR), ]
  if (nrow(batting_war) > count) {
    batting_war <- batting_war[1:count, ]
  }
  
  # Join with player and team names
  player_info <- players[, c("PlayerID", "FirstName", "LastName", "TeamID")]
  batting_war <- merge(batting_war, player_info, by = "PlayerID")
  batting_war <- merge(batting_war, teams[, c("TeamID", "TeamName")], by = "TeamID")
  
  # Format the result
  result <- data.frame(
    PlayerName = paste(batting_war$FirstName, batting_war$LastName),
    TeamName = batting_war$TeamName,
    "Season Batting WAR" = round(batting_war$sbWAR, 2)
  )
  
  return(result)
}

# Process Pitching WAR
process_pitching_war <- function(players, teams, pitching_data, league_avgs, count) {
  if (is.null(pitching_data) || nrow(pitching_data) == 0) {
    return(data.frame())
  }
  
  # Calculate pitching WAR for each player
  pitching_war <- data.frame(PlayerID = numeric(0), spWAR = numeric(0))
  
  for (i in seq_len(nrow(pitching_data))) {
    player_id <- pitching_data$PlayerID[i]
    player_stats <- pitching_data[i, ]
    
    # Calculate WAR
    war_value <- calculate_pWAR(player_stats, league_avgs)
    
    # Add to results
    pitching_war <- rbind(pitching_war, data.frame(PlayerID = player_id, spWAR = war_value))
  }
  
  # Sort by WAR and limit to count
  pitching_war <- pitching_war[order(-pitching_war$spWAR), ]
  if (nrow(pitching_war) > count) {
    pitching_war <- pitching_war[1:count, ]
  }
  
  # Join with player and team names
  player_info <- players[, c("PlayerID", "FirstName", "LastName", "TeamID")]
  pitching_war <- merge(pitching_war, player_info, by = "PlayerID")
  pitching_war <- merge(pitching_war, teams[, c("TeamID", "TeamName")], by = "TeamID")
  
  # Format the result
  result <- data.frame(
    PlayerName = paste(pitching_war$FirstName, pitching_war$LastName),
    TeamName = pitching_war$TeamName,
    "Season Pitching WAR" = round(pitching_war$spWAR, 2)
  )
  
  return(result)
}

# Process OPS+
process_ops_plus <- function(players, teams, batting_data, league_avgs, count) {
  if (is.null(batting_data) || nrow(batting_data) == 0) {
    return(data.frame())
  }
  
  # Filter players with minimum plate appearances
  batting_data <- batting_data[batting_data$PA >= 20, ]
  if (nrow(batting_data) == 0) {
    return(data.frame())
  }
  
  # Get league OPS
  league_ops <- league_avgs$batting$League_OPS
  if (is.null(league_ops) || is.na(league_ops) || league_ops == 0) {
    league_ops <- 0.720  # Default if no data
  }
  
  # Calculate OPS+ for each player
  ops_plus <- data.frame(PlayerID = numeric(0), OPS_Plus = numeric(0))
  
  for (i in seq_len(nrow(batting_data))) {
    player_id <- batting_data$PlayerID[i]
    player_stats <- batting_data[i, ]
    
    # Calculate OPS - adjusting for database schema
    # Calculate hits (H) as sum of 1B, 2B, 3B, HR
    h <- player_stats$`1B` + player_stats$`2B` + player_stats$`3B` + player_stats$HR
    bb <- player_stats$BB
    hbp <- player_stats$HBP
    singles <- player_stats$`1B`
    tb <- singles + (2 * player_stats$`2B`) + (3 * player_stats$`3B`) + (4 * player_stats$HR)
    # Calculate AB as PA - BB - HBP - SF - SH
    ab <- player_stats$PA - bb - hbp - player_stats$SF - player_stats$SH
    sf <- player_stats$SF
    
    obp <- (h + bb + hbp) / (ab + bb + hbp + sf)
    slg <- tb / ab
    ops <- obp + slg
    
    # Calculate OPS+
    ops_plus_value <- (ops / league_ops) * 100
    
    # Add to results
    ops_plus <- rbind(ops_plus, data.frame(PlayerID = player_id, OPS_Plus = ops_plus_value))
  }
  
  # Sort by OPS+ and limit to count
  ops_plus <- ops_plus[order(-ops_plus$OPS_Plus), ]
  if (nrow(ops_plus) > count) {
    ops_plus <- ops_plus[1:count, ]
  }
  
  # Join with player and team names
  player_info <- players[, c("PlayerID", "FirstName", "LastName", "TeamID")]
  ops_plus <- merge(ops_plus, player_info, by = "PlayerID")
  ops_plus <- merge(ops_plus, teams[, c("TeamID", "TeamName")], by = "TeamID")
  
  # Format the result
  result <- data.frame(
    PlayerName = paste(ops_plus$FirstName, ops_plus$LastName),
    TeamName = ops_plus$TeamName,
    "OPS+" = round(ops_plus$OPS_Plus, 0)
  )
  
  return(result)
}

# Process wRC (weighted Runs Created)
process_wrc <- function(players, teams, batting_data, count) {
  if (is.null(batting_data) || nrow(batting_data) == 0) {
    return(data.frame())
  }
  
  # Use centralized wRC from object_engine.R
  if (!exists("wRC", envir = .GlobalEnv)) {
    cat("wRC not found in global environment\n")
    return(data.frame())
  }
  wrc_vals <- get("wRC", envir = .GlobalEnv)
  
  # Merge wRC values into batting_data
  batting_data$wRC <- wrc_vals[match(batting_data$PlayerID, players$PlayerID)]
  
  # Sort and select top N
  batting_data <- batting_data[order(-batting_data$wRC), ]
  if (nrow(batting_data) > count) {
    batting_data <- batting_data[1:count, ]
  }
  
  # Merge player/team info
  batting_data <- merge(batting_data, players[, c("PlayerID", "FirstName", "LastName", "TeamID")], by = "PlayerID")
  batting_data <- merge(batting_data, teams[, c("TeamID", "TeamName")], by = "TeamID")
  
  # Format result
  result <- data.frame(
    PlayerName = paste(batting_data$FirstName, batting_data$LastName),
    TeamName = batting_data$TeamName,
    "wRC" = round(batting_data$wRC, 1)
  )
  
  return(result)
}

# Process ERA+
process_era_plus <- function(players, teams, pitching_data, count) {
  if (is.null(pitching_data) || nrow(pitching_data) == 0) {
    return(data.frame())
  }
  
  # Filter pitchers with minimum innings pitched (at least 3 innings)
  pitching_data <- pitching_data[pitching_data$OutsRecorded >= 9, ]
  if (nrow(pitching_data) == 0) {
    return(data.frame())
  }
  
  # Use centralized ERA_plus from object_engine.R
  if (!exists("ERA_plus", envir = .GlobalEnv)) {
    cat("ERA_plus not found in global environment\n")
    return(data.frame())
  }
  era_plus_vals <- get("ERA_plus", envir = .GlobalEnv)
  
  # Merge ERA+ values into pitching_data
  pitching_data$ERA_Plus <- era_plus_vals[match(pitching_data$PlayerID, players$PlayerID)]
  
  # Sort and select top N
  pitching_data <- pitching_data[order(-pitching_data$ERA_Plus), ]
  if (nrow(pitching_data) > count) {
    pitching_data <- pitching_data[1:count, ]
  }
  
  # Merge player/team info
  pitching_data <- merge(pitching_data, players[, c("PlayerID", "FirstName", "LastName", "TeamID")], by = "PlayerID")
  pitching_data <- merge(pitching_data, teams[, c("TeamID", "TeamName")], by = "TeamID")
  
  # Format result
  result <- data.frame(
    PlayerName = paste(pitching_data$FirstName, pitching_data$LastName),
    TeamName = pitching_data$TeamName,
    "ERA+" = round(pitching_data$ERA_Plus, 0)
  )
  
  return(result)
}

# Process OPS+
process_ops_plus <- function(players, teams, batting_data, count) {
  # SIMPLIFIED APPROACH: Directly use the global OPS_plus values
  
  # Check if OPS_plus exists in the global environment
  if (!exists("OPS_plus", envir = .GlobalEnv)) {
    cat("OPS_plus not found in global environment\n")
    return(data.frame())
  }
  
  # Get the global OPS_plus values
  ops_plus_values <- get("OPS_plus", envir = .GlobalEnv)
  
  # Create a data frame with PlayerID and OPS+ value
  if (!is.null(names(ops_plus_values))) {
    # If it's a named vector, use the names as PlayerIDs
    player_ids <- as.numeric(names(ops_plus_values))
    ops_plus_df <- data.frame(
      PlayerID = player_ids,
      OPS_Plus = as.numeric(ops_plus_values)
    )
  } else {
    # Otherwise, assume it aligns with players$PlayerID
    ops_plus_df <- data.frame(
      PlayerID = players$PlayerID[1:length(ops_plus_values)],
      OPS_Plus = as.numeric(ops_plus_values)
    )
  }
  
  # Filter out players with no batting stats (they'll have default value of 100)
  if (!is.null(batting_data) && nrow(batting_data) > 0) {
    # Only include players who have batting stats with minimum PA
    batting_data <- batting_data[batting_data$PA >= 10, ]
    ops_plus_df <- ops_plus_df[ops_plus_df$PlayerID %in% batting_data$PlayerID, ]
  }
  
  # Remove players with OPS+ of exactly 100 (likely default values)
  # But keep some if we'd otherwise have too few results
  non_default <- ops_plus_df[ops_plus_df$OPS_Plus != 100, ]
  if (nrow(non_default) >= count) {
    ops_plus_df <- non_default
  }
  
  # Sort by OPS+ (descending) and take top N
  ops_plus_df <- ops_plus_df[order(-ops_plus_df$OPS_Plus), ]
  if (nrow(ops_plus_df) > count) {
    ops_plus_df <- ops_plus_df[1:count, ]
  }
  
  # If we have no results, return empty data frame
  if (nrow(ops_plus_df) == 0) {
    return(data.frame())
  }
  
  # Merge with player and team info
  result_df <- merge(ops_plus_df, players[, c("PlayerID", "FirstName", "LastName", "TeamID")], by = "PlayerID")
  result_df <- merge(result_df, teams[, c("TeamID", "TeamName")], by = "TeamID")
  
  # Format the final result
  result <- data.frame(
    PlayerName = paste(result_df$FirstName, result_df$LastName),
    TeamName = result_df$TeamName,
    "OPS+" = round(result_df$OPS_Plus, 0)
  )
  
  return(result)
}

# Process FIP (Fielding Independent Pitching)
process_fip <- function(players, teams, pitching_data, count) {
  if (is.null(pitching_data) || nrow(pitching_data) == 0) {
    return(data.frame())
  }
  
  # Filter pitchers with minimum innings pitched (at least 3 innings)
  pitching_data <- pitching_data[pitching_data$OutsRecorded >= 9, ]
  if (nrow(pitching_data) == 0) {
    return(data.frame())
  }
  
  # Use centralized FIP from object_engine.R
  if (!exists("FIP", envir = .GlobalEnv)) {
    cat("FIP not found in global environment\n")
    return(data.frame())
  }
  fip_vals <- get("FIP", envir = .GlobalEnv)
  
  # Merge FIP values into pitching_data
  pitching_data$FIP <- fip_vals[match(pitching_data$PlayerID, players$PlayerID)]
  
  # Sort and select top N (lower is better)
  pitching_data <- pitching_data[order(pitching_data$FIP), ]
  if (nrow(pitching_data) > count) {
    pitching_data <- pitching_data[1:count, ]
  }
  
  # Merge player/team info
  pitching_data <- merge(pitching_data, players[, c("PlayerID", "FirstName", "LastName", "TeamID")], by = "PlayerID")
  pitching_data <- merge(pitching_data, teams[, c("TeamID", "TeamName")], by = "TeamID")
  
  # Format result
  result <- data.frame(
    PlayerName = paste(pitching_data$FirstName, pitching_data$LastName),
    TeamName = pitching_data$TeamName,
    "FIP" = round(pitching_data$FIP, 2)
  )
  
  return(result)
}

# Process Batting Average
process_batting_avg <- function(players, teams, batting_data, count) {
  if (is.null(batting_data) || nrow(batting_data) == 0) {
    return(data.frame())
  }
  
  # Filter batters with minimum at-bats (at least 10)
  batting_data <- batting_data[batting_data$AB >= 10, ]
  if (nrow(batting_data) == 0) {
    return(data.frame())
  }
  
  # Use centralized AVG from object_engine.R
  if (!exists("AVG", envir = .GlobalEnv)) {
    cat("AVG not found in global environment\n")
    return(data.frame())
  }
  avg_vals <- get("AVG", envir = .GlobalEnv)
  
  # Merge AVG values into batting_data
  batting_data$AVG <- avg_vals[match(batting_data$PlayerID, players$PlayerID)]
  
  # Sort and select top N
  batting_data <- batting_data[order(-batting_data$AVG), ]
  if (nrow(batting_data) > count) {
    batting_data <- batting_data[1:count, ]
  }
  
  # Merge player/team info
  batting_data <- merge(batting_data, players[, c("PlayerID", "FirstName", "LastName", "TeamID")], by = "PlayerID")
  batting_data <- merge(batting_data, teams[, c("TeamID", "TeamName")], by = "TeamID")
  
  # Format result
  result <- data.frame(
    PlayerName = paste(batting_data$FirstName, batting_data$LastName),
    TeamName = batting_data$TeamName,
    "Batting Average" = round(batting_data$AVG, 3)
  )
  
  return(result)
}
