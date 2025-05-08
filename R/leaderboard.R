# Fixed Leaderboard Module for TRCStatR
# This version removes any reactive code that might cause errors

# Helper function to create empty result with correct columns
create_empty_result <- function(stat_type) {
  # Create an empty dataframe with appropriate columns
  data <- data.frame(
    PlayerName = character(0),
    TeamName = character(0),
    Value = numeric(0)
  )
  
  # Rename the last column based on stat_type
  colnames(data)[3] <- switch(stat_type,
                             "TotalWAR" = "Season WAR",
                             "sbWAR" = "Season Batting WAR",
                             "spWAR" = "Season Pitching WAR",
                             "OPS_Plus" = "OPS+",
                             "wRC" = "wRC",
                             "ERA_Plus" = "ERA+",
                             "FIP" = "FIP",
                             "Batting Average")
  
  return(data)
}

# Helper functions for WAR calculations
calculate_bWAR <- function(player_stats, league_avgs, war_scale = 1.0) {
  # Simple batting WAR calculation
  # Based on total bases, walks, and stolen bases
  
  # Get league averages
  league_pa_per_game <- league_avgs$batting$League_PA_per_Game
  if (is.null(league_pa_per_game) || is.na(league_pa_per_game) || league_pa_per_game == 0) {
    league_pa_per_game <- 4.5  # Default if no data
  }
  
  league_tb_per_pa <- league_avgs$batting$League_TB_per_PA
  if (is.null(league_tb_per_pa) || is.na(league_tb_per_pa) || league_tb_per_pa == 0) {
    league_tb_per_pa <- 0.6  # Default if no data
  }
  
  league_bb_per_pa <- league_avgs$batting$League_BB_per_PA
  if (is.null(league_bb_per_pa) || is.na(league_bb_per_pa) || league_bb_per_pa == 0) {
    league_bb_per_pa <- 0.15  # Default if no data
  }
  
  # Calculate WAR components
  pa <- player_stats$PA
  if (is.null(pa) || is.na(pa) || pa == 0) return(0)
  
  # Adjust for playing time (162 game season equivalent)
  games_played_equivalent <- pa / league_pa_per_game
  season_factor <- 162 / games_played_equivalent
  
  # Calculate offensive value
  tb_value <- (player_stats$TB / pa - league_tb_per_pa) * pa
  bb_value <- (player_stats$BB / pa - league_bb_per_pa) * pa
  sb_value <- player_stats$SB * 0.2  # Simple value for stolen bases
  
  # Calculate raw WAR
  raw_war <- (tb_value + bb_value + sb_value) / 10
  
  # Scale to full season and apply WAR scale factor
  season_war <- raw_war * season_factor * war_scale
  
  # Cap extreme values
  return(min(max(season_war, -10), 15))
}

calculate_pWAR <- function(player_stats, league_avgs, war_scale = 1.0) {
  # Simple pitching WAR calculation
  # Based on ERA, K/9, and BB/9
  
  # Get league averages
  league_k_per_ip <- league_avgs$pitching$League_K_per_IP
  if (is.null(league_k_per_ip) || is.na(league_k_per_ip) || league_k_per_ip == 0) {
    league_k_per_ip <- 2.0  # Default if no data
  }
  
  league_bb_per_ip <- league_avgs$pitching$League_BB_per_IP
  if (is.null(league_bb_per_ip) || is.na(league_bb_per_ip) || league_bb_per_ip == 0) {
    league_bb_per_ip <- 1.0  # Default if no data
  }
  
  league_era <- league_avgs$pitching$League_ERA
  if (is.null(league_era) || is.na(league_era) || league_era == 0) {
    league_era <- 5.50  # Default if no data
  }
  
  # Calculate WAR components
  ip <- player_stats$IP
  if (is.null(ip) || is.na(ip) || ip == 0) return(0)
  
  # Adjust for playing time (162 game season equivalent)
  games_pitched_equivalent <- ip / 6  # Assuming 6 innings per game for youth baseball
  season_factor <- 162 / games_pitched_equivalent
  
  # Calculate ERA
  era <- 9 * player_stats$ER / ip
  
  # Calculate K/9 and BB/9
  k_per_9 <- 9 * player_stats$K / ip
  bb_per_9 <- 9 * player_stats$BB / ip
  
  # Calculate raw WAR components
  era_component <- (league_era - era) * ip / 9
  k_component <- (k_per_9 - league_k_per_ip) * ip / 9
  bb_component <- (league_bb_per_ip - bb_per_9) * ip / 9
  
  # Calculate raw WAR
  raw_war <- era_component + k_component + bb_component
  
  # Scale to full season and apply WAR scale factor
  season_war <- raw_war * season_factor * war_scale
  
  # Cap extreme values
  return(min(max(season_war, -10), 15))
}

# Generate Leaderboard function
generate_leaderboard <- function(stat_type, count) {
  # Set count to numeric and ensure it's reasonable
  count <- as.numeric(count)
  if (is.na(count) || count < 1) count <- 10
  if (count > 50) count <- 50
  
  # Get active division and season from settings (in-memory)
  if (exists("settings", envir = .GlobalEnv)) {
    settings <- get("settings", envir = .GlobalEnv)
  } else {

    return(create_empty_result(stat_type))
  }
  if (is.null(settings$ActiveDivisionID) || is.null(settings$ActiveSeasonYear)) {

    return(create_empty_result(stat_type))
  }
  division_id <- settings$ActiveDivisionID[1]
  season_year <- settings$ActiveSeasonYear[1]
  
  # Get league averages for WAR calculations (in-memory)
  league_avgs <- tryCatch({
    if (exists("league_averages", envir = .GlobalEnv)) {
      league_averages <- get("league_averages", envir = .GlobalEnv)
      list(
        batting = league_averages$batting,
        pitching = league_averages$pitching
      )
    } else if (exists("get_league_averages", envir = .GlobalEnv)) {
      league_avgs_fn <- get("get_league_averages", envir = .GlobalEnv)
      league_avgs_fn()
    } else {
      list(
        batting = data.frame(
          League_PA_per_Game = 4.5,
          League_TB_per_PA = 0.6,
          League_BB_per_PA = 0.15,
          League_OPS = 0.830
        ),
        pitching = data.frame(
          League_K_per_IP = 2.0,
          League_BB_per_IP = 1.0,
          League_ERA = 5.50
        )
      )
    }
  }, error = function(e) {

    list(
      batting = data.frame(
        League_PA_per_Game = 4.5,
        League_TB_per_PA = 0.6,
        League_BB_per_PA = 0.15,
        League_OPS = 0.830
      ),
      pitching = data.frame(
        League_K_per_IP = 2.0,
        League_BB_per_IP = 1.0,
        League_ERA = 5.50
      )
    )
  })
  
  # Retrieve player, team, batting, and pitching data (in-memory)
  if (!exists("players", envir = .GlobalEnv) || !exists("teams", envir = .GlobalEnv) ||
      !exists("batting_stats", envir = .GlobalEnv) || !exists("pitching_stats", envir = .GlobalEnv)) {

    return(create_empty_result(stat_type))
  }
  players <- get("players", envir = .GlobalEnv)
  teams <- get("teams", envir = .GlobalEnv)
  batting_data <- subset(get("batting_stats", envir = .GlobalEnv), DivisionID == division_id & SeasonYear == season_year & PA > 10)
  pitching_data <- subset(get("pitching_stats", envir = .GlobalEnv), DivisionID == division_id & SeasonYear == season_year & OutsRecorded >= 9)

  
  # Check if we have data
  if (is.null(players) || nrow(players) == 0) {

    return(create_empty_result(stat_type))
  }
  
  # Process data based on stat type
  result <- switch(stat_type,
    "TotalWAR" = process_total_war(players, teams, batting_data, pitching_data, league_avgs, count),
    "sbWAR" = process_batting_war(players, teams, batting_data, league_avgs, count),
    "spWAR" = process_pitching_war(players, teams, pitching_data, league_avgs, count),
    "OPS_Plus" = process_ops_plus(players, teams, batting_data, count),
    "wRC" = process_wrc(players, teams, batting_data, count),
    "ERA_Plus" = process_era_plus(players, teams, pitching_data, count),
    "FIP" = process_fip(players, teams, pitching_data, count),
    "AVG" = process_batting_avg(players, teams, batting_data, count),
    create_empty_result(stat_type) # Default case
  )
  
  # If result is empty, return an empty result with appropriate columns
  if (is.null(result) || nrow(result) == 0) {
    return(create_empty_result(stat_type))
  }
  
  return(result)
}
