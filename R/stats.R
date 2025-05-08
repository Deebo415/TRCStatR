# stats.R
# This file contains specialized statistical analysis functions that complement
# the core statistics provided by object_engine.R

# Import required packages
library(dplyr)
library(tidyr)

# Import safe functions from object_engine.R
# This assumes object_engine.R is properly sourced before stats.R
# NULL-handling operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Define global variables to avoid lint warnings
utils::globalVariables(c(
  # Data frames
  "batting_stats", "pitching_stats", 
  
  # ID fields
  "PlayerID", "TeamID", "GameID", "GameDate", "OpponentID", 
  
  # Game context
  "HomeAway", "DayNight", "Decision", 
  
  # Batting stats
  "PA", "AB", "BB", "HBP", "SF", "SH", "X1B", "X2B", "X3B", "HR", "H", "RBI", "K", "SB", "CS",
  
  # Pitching stats
  "ER", "OutsRecorded", "BF", "IP", "GB", "FB", 
  
  # Calculated stats
  "OBP", "SLG", "AVG", "OPS", "ISO", "ERA", "WHIP", "K_per_9", "BB_per_9", "HR_per_9",
  "GB_Fly_Ratio", "GB_Percent", "GB_pct", "K_pct", "BB_pct", "SB_Success",
  
  # Boolean flags
  "quality_start", "win", "loss", "save", "hold", "no_er", "multi_k",
  "hit_game", "multi_hit_game", "hr_game", "multi_hr_game", "rbi_game", "multi_rbi_game", 
  "bb_game", "multi_bb_game", "k_game", "multi_k_game", "sb_game", "multi_sb_game",
  
  # dplyr functions
  "n",
  
  # Helper functions from object_engine.R
  "safe_divide", "safe_batting_avg", "safe_obp", "safe_slg", "safe_era", "safe_whip", "safe_per_nine",
  "calc_current_streak", "calc_longest_streak", "get_player_stats", "get_team_stats"
))

# Define safe functions if they're not already available
# These should be imported from object_engine.R in practice
if (!exists("safe_divide")) {
  #' Safely divide two numbers, returning 0 for NA or division by zero
  #' @param numerator The numerator in the division
  #' @param denominator The denominator in the division
  #' @return The result of division, or 0 if denominator is 0 or NA
  #' @export
  safe_divide <- function(numerator, denominator) {
    ifelse(is.na(denominator) | denominator == 0, 0, numerator / denominator)
  }
}

if (!exists("safe_batting_avg")) {
  #' Calculate batting average safely
  #' @param hits Total hits
  #' @param at_bats Total at bats
  #' @return Batting average, or 0 if at_bats is 0 or NA
  #' @export
  safe_batting_avg <- function(hits, at_bats) {
    safe_divide(hits, at_bats)
  }
}

if (!exists("safe_obp")) {
  #' Calculate on-base percentage safely
  #' @param hits Total hits
  #' @param walks Total walks
  #' @param hit_by_pitch Total hit by pitch
  #' @param at_bats Total at bats
  #' @param sacrifice_flies Total sacrifice flies
  #' @return On-base percentage, or 0 if denominator is 0 or NA
  #' @export
  safe_obp <- function(hits, walks, hit_by_pitch, at_bats, sacrifice_flies) {
    safe_divide(hits + walks + hit_by_pitch, at_bats + walks + hit_by_pitch + sacrifice_flies)
  }
}

if (!exists("safe_slg")) {
  #' Calculate slugging percentage safely
  #' @param total_bases Total bases
  #' @param at_bats Total at bats
  #' @return Slugging percentage, or 0 if at_bats is 0 or NA
  #' @export
  safe_slg <- function(total_bases, at_bats) {
    safe_divide(total_bases, at_bats)
  }
}

if (!exists("safe_era")) {
  #' Calculate earned run average safely
  #' @param earned_runs Total earned runs
  #' @param innings_pitched Total innings pitched
  #' @return ERA, or 0 if innings_pitched is 0 or NA
  #' @export
  safe_era <- function(earned_runs, innings_pitched) {
    safe_divide(9 * earned_runs, innings_pitched)
  }
}

if (!exists("safe_whip")) {
  #' Calculate WHIP safely
  #' @param walks Total walks
  #' @param hits Total hits
  #' @param hit_by_pitch Total hit by pitch
  #' @param innings_pitched Total innings pitched
  #' @return WHIP, or 0 if innings_pitched is 0 or NA
  #' @export
  safe_whip <- function(walks, hits, hit_by_pitch, innings_pitched) {
    safe_divide(walks + hits + hit_by_pitch, innings_pitched)
  }
}

if (!exists("safe_per_nine")) {
  #' Calculate per-nine-inning rate safely
  #' @param stat The stat to calculate rate for
  #' @param innings_pitched Total innings pitched
  #' @return Per-nine-inning rate, or 0 if innings_pitched is 0 or NA
  #' @export
  safe_per_nine <- function(stat, innings_pitched) {
    safe_divide(9 * stat, innings_pitched)
  }
}

# Helper function to calculate current streak
calc_current_streak <- function(bool_vector) {
  if (length(bool_vector) == 0) return(0)
  
  # If the last value is FALSE, current streak is 0
  if (!bool_vector[length(bool_vector)]) return(0)
  
  # Count consecutive TRUE values from the end
  streak <- 0
  for (i in rev(seq_along(bool_vector))) {
    if (bool_vector[i]) {
      streak <- streak + 1
    } else {
      break
    }
  }
  return(streak)
}

# Helper function to calculate longest streak
calc_longest_streak <- function(bool_vector) {
  if (length(bool_vector) == 0) return(0)
  
  max_streak <- 0
  current <- 0
  
  for (val in bool_vector) {
    if (val) {
      current <- current + 1
      max_streak <- max(max_streak, current)
    } else {
      current <- 0
    }
  }
  return(max_streak)
}

# Helper function to calculate player progress stats (time-series analysis)
calculate_player_progress_stats <- function(player_id, 
                                            batting_stats_df = batting_stats, 
                                            pitching_stats_df = pitching_stats) {
  tryCatch({
    cat("Calculating progress stats for player:", player_id, "\n")
    
    # Get player's stats
    player_batting <- batting_stats_df %>%
      filter(PlayerID == player_id) %>%
      arrange(GameDate) %>%
      mutate(
        # Use X1B, X2B, X3B naming convention to match object_engine.R
        AVG = safe_batting_avg(cumsum(X1B + X2B + X3B + HR), cumsum(PA - BB - HBP - SF - SH)),
        OBP = safe_obp(cumsum(X1B + X2B + X3B + HR), cumsum(BB), cumsum(HBP), cumsum(PA - BB - HBP - SF - SH), cumsum(SF)),
        SLG = safe_slg(cumsum(X1B + 2 * X2B + 3 * X3B + 4 * HR), cumsum(PA - BB - HBP - SF - SH)),
        OPS = OBP + SLG,
        ISO = safe_divide(cumsum(X1B + 2 * X2B + 3 * X3B + 4 * HR - (X1B + X2B + X3B + HR)), cumsum(PA - BB - HBP - SF - SH)),
        BB_pct = safe_divide(cumsum(BB), cumsum(PA)),
        K_pct = safe_divide(cumsum(K), cumsum(PA))
      )
    
    player_pitching <- pitching_stats_df %>%
      filter(PlayerID == player_id) %>%
      arrange(GameDate) %>%
      mutate(
        ERA = safe_era(cumsum(ER), cumsum(OutsRecorded / 3)),
        WHIP = safe_whip(cumsum(BB), cumsum(H), cumsum(HBP), cumsum(OutsRecorded / 3)),
        K_per_9 = safe_per_nine(cumsum(K), cumsum(OutsRecorded / 3)),
        BB_per_9 = safe_per_nine(cumsum(BB), cumsum(OutsRecorded / 3)),
        HR_per_9 = safe_per_nine(cumsum(HR), cumsum(OutsRecorded / 3)),
        GB_Fly_Ratio = safe_divide(cumsum(GB), cumsum(FB)),
        GB_Percent = 100 * safe_divide(cumsum(GB), (cumsum(GB) + cumsum(FB)))
      )
    
    return(list(
      batting = player_batting,
      pitching = player_pitching
    ))
    
  }, error = function(e) {
    cat("ERROR calculating player progress stats:", e$message, "\n")
    return(NULL)
  })
}

# Helper function to calculate team progress stats (time-series analysis)
calculate_team_progress_stats <- function(team_id, 
                                          batting_stats_df = batting_stats, 
                                          pitching_stats_df = pitching_stats) {
  tryCatch({
    cat("Calculating progress stats for team:", team_id, "\n")
    
    # Get team's stats
    team_batting <- batting_stats_df %>%
      filter(TeamID == team_id) %>%
      arrange(GameDate) %>%
      group_by(GameDate) %>%
      summarize(
        # Use X1B, X2B, X3B naming convention to match object_engine.R
        AVG = safe_batting_avg(cumsum(X1B + X2B + X3B + HR), cumsum(PA - BB - HBP - SF - SH)),
        OBP = safe_obp(cumsum(X1B + X2B + X3B + HR), cumsum(BB), cumsum(HBP), cumsum(PA - BB - HBP - SF - SH), cumsum(SF)),
        SLG = safe_slg(cumsum(X1B + 2 * X2B + 3 * X3B + 4 * HR), cumsum(PA - BB - HBP - SF - SH)),
        OPS = OBP + SLG,
        ISO = safe_divide(cumsum(X1B + 2 * X2B + 3 * X3B + 4 * HR - (X1B + X2B + X3B + HR)), cumsum(PA - BB - HBP - SF - SH)),
        BB_pct = safe_divide(cumsum(BB), cumsum(PA)),
        K_pct = safe_divide(cumsum(K), cumsum(PA))
      )
    
    team_pitching <- pitching_stats_df %>%
      filter(TeamID == team_id) %>%
      arrange(GameDate) %>%
      group_by(GameDate) %>%
      summarize(
        ERA = safe_era(cumsum(ER), cumsum(OutsRecorded / 3)),
        WHIP = safe_whip(cumsum(BB), cumsum(H), cumsum(HBP), cumsum(OutsRecorded / 3)),
        K_per_9 = safe_per_nine(cumsum(K), cumsum(OutsRecorded / 3)),
        BB_per_9 = safe_per_nine(cumsum(BB), cumsum(OutsRecorded / 3)),
        HR_per_9 = safe_per_nine(cumsum(HR), cumsum(OutsRecorded / 3)),
        GB_Fly_Ratio = safe_divide(cumsum(GB), cumsum(FB)),
        GB_Percent = 100 * safe_divide(cumsum(GB), (cumsum(GB) + cumsum(FB)))
      )
    
    return(list(
      batting = team_batting,
      pitching = team_pitching
    ))
    
  }, error = function(e) {
    cat("ERROR calculating team progress stats:", e$message, "\n")
    return(NULL)
  })
}

# Helper function to calculate player comparison stats
calculate_player_comparison_stats <- function(player1_id, player2_id, 
                                              batting_stats_df = batting_stats, 
                                              pitching_stats_df = pitching_stats) {
  tryCatch({
    cat("Calculating comparison stats for players:", player1_id, "and", player2_id, "\n")
    
    # Get player 1 stats using object_engine.R
    player1_stats <- get_player_stats(player1_id)
    # Get player 2 stats using object_engine.R
    player2_stats <- get_player_stats(player2_id)
    
    if (is.null(player1_stats) || is.null(player2_stats)) {
      return(NULL)
    }
    
    # Calculate differences in batting stats
    batting_diff <- list(
      AVG = player1_stats$AVG - player2_stats$AVG,
      OBP = player1_stats$OBP - player2_stats$OBP,
      SLG = player1_stats$SLG - player2_stats$SLG,
      OPS = player1_stats$OPS - player2_stats$OPS,
      ISO = player1_stats$ISO - player2_stats$ISO,
      BB_pct = player1_stats$BB_pct - player2_stats$BB_pct,
      K_pct = player1_stats$K_pct - player2_stats$K_pct,
      wOBA = player1_stats$wOBA - player2_stats$wOBA,
      wRC_plus = player1_stats$wRC_plus - player2_stats$wRC_plus,
      sbWAR = player1_stats$sbWAR - player2_stats$sbWAR
    )
    
    # Only calculate pitching differences if both players have pitching stats
    pitching_diff <- NULL
    if (!is.null(player1_stats$ERA) && !is.null(player2_stats$ERA)) {
      pitching_diff <- list(
        ERA = player1_stats$ERA - player2_stats$ERA,
        ERA_plus = player1_stats$ERA_plus - player2_stats$ERA_plus,
        WHIP = player1_stats$WHIP - player2_stats$WHIP,
        WHIP_plus = player1_stats$WHIP_plus - player2_stats$WHIP_plus,
        K_per_9 = player1_stats$K_per_9 - player2_stats$K_per_9,
        BB_per_9 = player1_stats$BB_per_9 - player2_stats$BB_per_9,
        K_BB_ratio = player1_stats$K_BB_ratio - player2_stats$K_BB_ratio,
        FIP = player1_stats$FIP - player2_stats$FIP,
        spWAR = player1_stats$spWAR - player2_stats$spWAR
      )
    }
    
    return(list(
      player1 = player1_stats,
      player2 = player2_stats,
      differences = list(
        batting = batting_diff,
        pitching = pitching_diff
      )
    ))
    
  }, error = function(e) {
    cat("ERROR calculating player comparison stats:", e$message, "\n")
    return(NULL)
  })
}

# Helper function to calculate team comparison stats
calculate_team_comparison_stats <- function(team1_id, team2_id, 
                                            batting_stats_df = batting_stats, 
                                            pitching_stats_df = pitching_stats) {
  tryCatch({
    cat("Calculating comparison stats for teams:", team1_id, "and", team2_id, "\n")
    
    # Get team 1 stats using object_engine.R
    team1_stats <- get_team_stats(team1_id)
    # Get team 2 stats using object_engine.R
    team2_stats <- get_team_stats(team2_id)
    
    if (is.null(team1_stats) || is.null(team2_stats)) {
      return(NULL)
    }
    
    # Calculate differences for batting stats
    batting_diff <- list(
      AVG = team1_stats$AVG - team2_stats$AVG,
      OBP = team1_stats$OBP - team2_stats$OBP,
      SLG = team1_stats$SLG - team2_stats$SLG,
      OPS = team1_stats$OPS - team2_stats$OPS,
      ISO = team1_stats$ISO - team2_stats$ISO,
      BB_rate = team1_stats$BB_rate - team2_stats$BB_rate,
      K_rate = team1_stats$K_rate - team2_stats$K_rate
    )
    
    # Calculate differences for pitching stats
    pitching_diff <- list(
      ERA = team1_stats$ERA - team2_stats$ERA,
      WHIP = team1_stats$WHIP - team2_stats$WHIP,
      K_per_9 = team1_stats$K_per_9 - team2_stats$K_per_9,
      BB_per_9 = team1_stats$BB_per_9 - team2_stats$BB_per_9,
      HR_per_9 = team1_stats$HR_per_9 - team2_stats$HR_per_9,
      K_BB_ratio = team1_stats$K_BB_ratio - team2_stats$K_BB_ratio
    )
    
    return(list(
      team1 = team1_stats,
      team2 = team2_stats,
      differences = list(
        batting = batting_diff,
        pitching = pitching_diff
      )
    ))
    
  }, error = function(e) {
    cat("ERROR calculating team comparison stats:", e$message, "\n")
    return(NULL)
  })
}

# Helper function to get player splits (home/away, day/night, vs opponent)
get_player_splits <- function(player_id, 
                              batting_stats_df = batting_stats, 
                              pitching_stats_df = pitching_stats) {
  tryCatch({
    cat("Getting splits for player:", player_id, "\n")
    
    # Get player's splits
    player_batting <- batting_stats_df %>%
      filter(PlayerID == player_id) %>%
      group_by(HomeAway, DayNight, OpponentID) %>%
      summarize(
        AVG = safe_batting_avg(sum(X1B + X2B + X3B + HR), sum(PA - BB - HBP - SF - SH)),
        OBP = safe_obp(sum(X1B + X2B + X3B + HR), sum(BB), sum(HBP), sum(PA - BB - HBP - SF - SH), sum(SF)),
        SLG = safe_slg(sum(X1B + 2 * X2B + 3 * X3B + 4 * HR), sum(PA - BB - HBP - SF - SH)),
        OPS = OBP + SLG,
        ISO = safe_divide(sum(X1B + 2 * X2B + 3 * X3B + 4 * HR - (X1B + X2B + X3B + HR)), sum(PA - BB - HBP - SF - SH)),
        BB_pct = safe_divide(sum(BB), sum(PA)),
        K_pct = safe_divide(sum(K), sum(PA)),
        SB_Success = safe_divide(sum(SB), sum(SB + CS)),
        n = n()
      )
    
    player_pitching <- pitching_stats_df %>%
      filter(PlayerID == player_id) %>%
      group_by(HomeAway, DayNight, OpponentID) %>%
      summarize(
        ERA = safe_era(sum(ER), sum(OutsRecorded / 3)),
        WHIP = safe_whip(sum(BB), sum(H), sum(HBP), sum(OutsRecorded / 3)),
        K_per_9 = safe_per_nine(sum(K), sum(OutsRecorded / 3)),
        BB_per_9 = safe_per_nine(sum(BB), sum(OutsRecorded / 3)),
        HR_per_9 = safe_per_nine(sum(HR), sum(OutsRecorded / 3)),
        GB_Fly_Ratio = safe_divide(sum(GB), sum(FB)),
        GB_pct = 100 * safe_divide(sum(GB), (sum(GB) + sum(FB))),
        n = n()
      )
    
    return(list(
      batting = player_batting,
      pitching = player_pitching
    ))
    
  }, error = function(e) {
    cat("ERROR getting player splits:", e$message, "\n")
    return(NULL)
  })
}

# Helper function to get team splits (home/away, day/night, vs opponent)
get_team_splits <- function(team_id, 
                            batting_stats_df = batting_stats, 
                            pitching_stats_df = pitching_stats) {
  tryCatch({
    cat("Getting splits for team:", team_id, "\n")
    
    # Get team's splits
    team_batting <- batting_stats_df %>%
      filter(TeamID == team_id) %>%
      group_by(HomeAway, DayNight, OpponentID) %>%
      summarize(
        AVG = safe_batting_avg(sum(X1B + X2B + X3B + HR), sum(PA - BB - HBP - SF - SH)),
        OBP = safe_obp(sum(X1B + X2B + X3B + HR), sum(BB), sum(HBP), sum(PA - BB - HBP - SF - SH), sum(SF)),
        SLG = safe_slg(sum(X1B + 2 * X2B + 3 * X3B + 4 * HR), sum(PA - BB - HBP - SF - SH)),
        OPS = OBP + SLG,
        ISO = safe_divide(sum(X1B + 2 * X2B + 3 * X3B + 4 * HR - (X1B + X2B + X3B + HR)), sum(PA - BB - HBP - SF - SH)),
        BB_pct = safe_divide(sum(BB), sum(PA)),
        K_pct = safe_divide(sum(K), sum(PA)),
        SB_Success = safe_divide(sum(SB), sum(SB + CS)),
        n = n()
      )
    
    team_pitching <- pitching_stats_df %>%
      filter(TeamID == team_id) %>%
      group_by(HomeAway, DayNight, OpponentID) %>%
      summarize(
        ERA = safe_era(sum(ER), sum(OutsRecorded / 3)),
        WHIP = safe_whip(sum(BB), sum(H), sum(HBP), sum(OutsRecorded / 3)),
        K_per_9 = safe_per_nine(sum(K), sum(OutsRecorded / 3)),
        BB_per_9 = safe_per_nine(sum(BB), sum(OutsRecorded / 3)),
        HR_per_9 = safe_per_nine(sum(HR), sum(OutsRecorded / 3)),
        GB_Fly_Ratio = safe_divide(sum(GB), sum(FB)),
        GB_pct = 100 * safe_divide(sum(GB), (sum(GB) + sum(FB))),
        n = n()
      )
    
    return(list(
      batting = team_batting,
      pitching = team_pitching
    ))
    
  }, error = function(e) {
    cat("ERROR getting team splits:", e$message, "\n")
    return(NULL)
  })
}

# Helper function to get player vs pitcher stats
get_player_vs_pitcher_stats <- function(batter_id, pitcher_id, 
                                        batting_stats_df = batting_stats, 
                                        pitching_stats_df = pitching_stats) {
  tryCatch({
    cat("Getting player vs pitcher stats for batter:", batter_id, "vs pitcher:", pitcher_id, "\n")
    
    # Get relevant games
    games <- intersect(
      unique(batting_stats_df$GameID[batting_stats_df$PlayerID == batter_id]),
      unique(pitching_stats_df$GameID[pitching_stats_df$PlayerID == pitcher_id])
    )
    
    if (length(games) == 0) {
      return(NULL)
    }
    
    # Get stats from these games
    batter_stats <- batting_stats_df %>%
      filter(GameID %in% games, PlayerID == batter_id) %>%
      summarize(
        PA = sum(PA),
        H = sum(X1B + X2B + X3B + HR),
        X1B = sum(X1B),
        X2B = sum(X2B),
        X3B = sum(X3B),
        HR = sum(HR),
        BB = sum(BB),
        K = sum(K),
        HBP = sum(HBP),
        SF = sum(SF),
        SH = sum(SH),
        AVG = safe_batting_avg(sum(X1B + X2B + X3B + HR), sum(PA - BB - HBP - SF - SH)),
        OBP = safe_obp(sum(X1B + X2B + X3B + HR), sum(BB), sum(HBP), sum(PA - BB - HBP - SF - SH), sum(SF)),
        SLG = safe_slg(sum(X1B + 2 * X2B + 3 * X3B + 4 * HR), sum(PA - BB - HBP - SF - SH)),
        OPS = OBP + SLG
      )
    
    pitcher_stats <- pitching_stats_df %>%
      filter(GameID %in% games, PlayerID == pitcher_id) %>%
      summarize(
        IP = sum(OutsRecorded) / 3,
        H = sum(H),
        ER = sum(ER),
        BB = sum(BB),
        K = sum(K),
        HR = sum(HR),
        HBP = sum(HBP),
        ERA = safe_era(sum(ER), sum(OutsRecorded) / 3),
        WHIP = safe_whip(sum(BB), sum(H), sum(HBP), sum(OutsRecorded) / 3),
        K_per_9 = safe_per_nine(sum(K), sum(OutsRecorded) / 3),
        BB_per_9 = safe_per_nine(sum(BB), sum(OutsRecorded) / 3)
      )
    
    return(list(
      batter = batter_stats,
      pitcher = pitcher_stats,
      games = length(games)
    ))
    
  }, error = function(e) {
    cat("ERROR getting player streaks:", e$message, "\n")
    return(NULL)
  })
}

# Helper function to get team vs team stats
get_team_vs_team_stats <- function(team1_id, team2_id, 
                                   batting_stats_df = batting_stats, 
                                   pitching_stats_df = pitching_stats) {
  tryCatch({
    cat("Getting team vs team stats for team:", team1_id, "vs team:", team2_id, "\n")
    
    # Get relevant games
    games <- intersect(
      unique(batting_stats_df$GameID[batting_stats_df$TeamID == team1_id]),
      unique(batting_stats_df$GameID[batting_stats_df$TeamID == team2_id])
    )
    
    if (length(games) == 0) {
      return(NULL)
    }
    
    # Get stats from these games
    team1_stats <- batting_stats_df %>%
      filter(GameID %in% games, TeamID == team1_id) %>%
      summarize(
        AVG = safe_batting_avg(sum(X1B + X2B + X3B + HR), sum(PA - BB - HBP - SF - SH)),
        OBP = safe_obp(sum(X1B + X2B + X3B + HR), sum(BB), sum(HBP), sum(PA - BB - HBP - SF - SH), sum(SF)),
        SLG = safe_slg(sum(X1B + 2 * X2B + 3 * X3B + 4 * HR), sum(PA - BB - HBP - SF - SH)),
        OPS = OBP + SLG,
        ISO = safe_divide(sum(X1B + 2 * X2B + 3 * X3B + 4 * HR - (X1B + X2B + X3B + HR)), sum(PA - BB - HBP - SF - SH)),
        BB_pct = safe_divide(sum(BB), sum(PA)),
        K_pct = safe_divide(sum(K), sum(PA)),
        SB_Success = safe_divide(sum(SB), sum(SB + CS)),
        n = n()
      )
    
    team2_stats <- pitching_stats_df %>%
      filter(GameID %in% games, TeamID == team2_id) %>%
      summarize(
        ERA = safe_era(sum(ER), sum(OutsRecorded / 3)),
        WHIP = safe_whip(sum(BB), sum(H), sum(HBP), sum(OutsRecorded / 3)),
        K_per_9 = safe_per_nine(sum(K), sum(OutsRecorded / 3)),
        BB_per_9 = safe_per_nine(sum(BB), sum(OutsRecorded / 3)),
        HR_per_9 = safe_per_nine(sum(HR), sum(OutsRecorded / 3)),
        GB_Fly_Ratio = safe_divide(sum(GB), sum(FB)),
        GB_pct = 100 * safe_divide(sum(GB), (sum(GB) + sum(FB))),
        n = n()
      )
    
    return(list(
      team1 = team1_stats,
      team2 = team2_stats
    ))
    
  }, error = function(e) {
    cat("ERROR getting team vs team stats:", e$message, "\n")
    return(NULL)
  })
}