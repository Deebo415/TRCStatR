# reports.R
# This file contains shared utilities for report generation
# Most stat calculation has been moved to object_engine.R
#
# This module expects data from object_engine.R in the following structure:
# - get_team_stats(): A function that returns a list with batting and pitching stats
# - get_team_leaders(): A function that returns top players in various stat categories
# - get_team_logo(): A function that returns the path to a team's logo file

# Load required packages
library(shiny)  # For UI components like div, span, h2, h3, etc.

# Declare global variables to avoid lint warnings
utils::globalVariables(c(
  # Data frame columns
  "TeamID", "TeamName", "IsComplete", "HomeTeamID", "AwayTeamID", "HomeScore", "AwayScore",
  "DisplayName", "OPS_Plus", "ERA_Plus", "sbWAR", "spWAR", "DivisionID",
  
  # Global data frames
  "players", "teams", "batting_stats", "pitching_stats", "games", "league_stats",
  
  # Functions from object_engine.R
  "get_team_stats", "get_team_leaders", "get_team_logo", "get_team_rankings", "format_stat"
))

# Define format_avg_stat function to ensure it's available
format_avg_stat <- function(avg) {
  if (is.null(avg) || is.na(avg) || !is.numeric(avg)) {
    return(".000")
  }
  # Format as 3-digit decimal with leading zero if needed
  return(sprintf(".%03d", round(avg * 1000)))
}

# Define fallback functions for object_engine.R functions in case they're not available
if (!exists("get_team_logo")) {
  get_team_logo <- function(team_name) {
    # Default fallback for team logo
    return("www/img/default_logo.png")
  }
}

if (!exists("get_team_stats")) {
  get_team_stats <- function(team_id) {
    warning("get_team_stats function not found, using fallback")
    return(list(
      batting = list(AVG = 0, OBP = 0, SLG = 0, OPS = 0, OPS_plus = 100, SB = 0),
      pitching = list(ERA = 0, ERA_plus = 100, WHIP = 0, WHIP_plus = 100, FIP = 0, K_per_9 = 0)
    ))
  }
}

if (!exists("get_team_leaders")) {
  get_team_leaders <- function(team_id) {
    warning("get_team_leaders function not found, using fallback")
    return(list())
  }
}

if (!exists("get_team_rankings")) {
  get_team_rankings <- function(team_id, division_id) {
    warning("get_team_rankings function not found, using fallback")
    return(list())
  }
}

# Helper function to format team CSS class
format_team_css_class <- function(team_name) {
  formatted <- tolower(gsub("[^a-zA-Z0-9]", "-", team_name))
  paste0("team-", formatted)
}

# Team snapshot generation function
generate_team_snapshot <- function(team_id, teams_df) {
  result <- tryCatch({
    snapshot <- generate_team_dashboard(team_id, teams_df)
    cat("Team snapshot generated successfully\n")
    return(snapshot)
  }, error = function(e) {
    cat("\n==== ERROR GENERATING TEAM SNAPSHOT ====\n")
    cat("Error message:", e$message, "\n")
    if (!is.null(e$call)) cat("Call:", deparse(e$call), "\n")
    cat("Team ID:", team_id, "\n")
    cat("=======================================\n")
    
    # Try to get team name for better error context
    team_name_text <- tryCatch({
      if (!is.null(teams_df) && is.data.frame(teams_df)) {
        team_row <- teams_df[teams_df$TeamID == team_id, ]
        if (nrow(team_row) > 0 && !is.null(team_row$TeamName)) {
          as.character(team_row$TeamName[1])
        } else {
          paste("Team ID:", team_id)
        }
      } else {
        paste("Team ID:", team_id)
      }
    }, error = function(err) {
      paste("Team ID:", team_id)
    })
    
    # Return a user-friendly error message
    return(div(
      class = "team-snapshot error",
      style = "border: 1px solid #f8d7da; border-radius: 8px; padding: 15px; margin-bottom: 20px; background-color: #f8d7da; color: #721c24;",
      h3("Error Generating Team Snapshot", style = "margin-top: 0; color: #721c24;"),
      p(paste("Error occurred while generating snapshot for", team_name_text), style = "margin-bottom: 10px;"),
      p(as.character(e$message), style = "font-family: monospace; background-color: #f2f2f2; padding: 8px; border-radius: 4px;"),
      p("Please check the console for more details.", style = "font-style: italic;")
    ))
  })
  
  cat("==== DEBUG: TEAM SNAPSHOT GENERATION END ====\n")
  return(result)
}

# Function to generate team dashboard
# This now uses object_engine.R functions for data retrieval
generate_team_dashboard <- function(team_id, teams_df) {
  team_id_numeric <- as.integer(team_id)
  cat("Processing team snapshot for team ID:", team_id_numeric, "\n")
  
  # Get team info
  team_info <- teams_df[teams_df$TeamID == team_id_numeric, ]
  
  # Validate team info
  if (is.null(team_info) || nrow(team_info) == 0) {
    cat("ERROR: No team found with ID:", team_id_numeric, "\n")
    stop(paste("No team found with ID:", team_id_numeric))
  }
  
  cat("Team info retrieved:", as.character(team_info$TeamName[1]), "\n")
  
  # Get team name and format for CSS
  team_name <- as.character(team_info$TeamName[1])
  team_css_class <- format_team_css_class(team_name)
  
  # Format team name for logo file with error handling
  logo_filename <- tryCatch({
    if (exists("get_team_logo", mode = "function")) {
      get_team_logo(team_name)
    } else {
      warning("get_team_logo function not found, using fallback")
      "www/img/default_logo.png"
    }
  }, error = function(e) {
    warning(paste("Error getting team logo:", e$message))
    "www/img/default_logo.png"
  })
  cat("Logo filename:", logo_filename, "\n")
  
  # Get team stats from object_engine.R with error handling
  cat("Retrieving team stats from object_engine...\n")
  team_stats <- tryCatch({
    if (exists("get_team_stats", mode = "function")) {
      get_team_stats(team_id_numeric)
    } else {
      warning("get_team_stats function not found, using fallback")
      list(
        batting = list(AVG = 0, OBP = 0, SLG = 0, OPS = 0, OPS_plus = 100, SB = 0),
        pitching = list(ERA = 0, ERA_plus = 100, WHIP = 0, WHIP_plus = 100, FIP = 0, K_per_9 = 0)
      )
    }
  }, error = function(e) {
    warning(paste("Error getting team stats:", e$message))
    list(
      batting = list(AVG = 0, OBP = 0, SLG = 0, OPS = 0, OPS_plus = 100, SB = 0),
      pitching = list(ERA = 0, ERA_plus = 100, WHIP = 0, WHIP_plus = 100, FIP = 0, K_per_9 = 0)
    )
  })
  
  if (is.null(team_stats)) {
    cat("ERROR: Failed to retrieve team stats\n")
    return(div(
      class = "alert alert-danger",
      h3("No Stats Available"),
      p("No statistics were found for this team.")
    ))
  }
  
  # Extract stats from the team_stats object
  batting_stats <- team_stats$batting
  pitching_stats <- team_stats$pitching
  
  cat("Team stats retrieved successfully\n")
  cat("Batting stats keys:", paste(names(batting_stats), collapse=", "), "\n")
  cat("Pitching stats keys:", paste(names(pitching_stats), collapse=", "), "\n")
  
  # Get team players (this is used for reference but might be needed in future extensions)
  team_players <- players[players$TeamID == team_id_numeric, ]  # nolint
  
  # Get team games to count played games and runs scored
  cat("Getting team games...\n")
  # Safely access games data frame
  team_games <- tryCatch({
    if (exists("games") && is.data.frame(get("games"))) {
      games_df <- get("games")
      games_df[games_df$HomeTeamID == team_id_numeric | games_df$AwayTeamID == team_id_numeric, ]
    } else {
      warning("Games data frame not found or not a data frame")
      NULL
    }
  }, error = function(e) {
    warning(paste("Error accessing games data:", e$message))
    NULL
  })
  
  # Calculate team runs per game
  total_runs <- 0
  games_played <- 0
  
  if (!is.null(team_games) && nrow(team_games) > 0) {
    games_played <- nrow(team_games)
    cat("Found", games_played, "games for team", team_name, "\n")
    
    for (i in seq_len(nrow(team_games))) {
      game <- team_games[i,]
      
      # Add runs scored by this team
      if (game$HomeTeamID == team_id_numeric) {
        total_runs <- total_runs + as.numeric(game$HomeScore)
      } else {
        total_runs <- total_runs + as.numeric(game$AwayScore)
      }
    }
  } else {
    cat("WARNING: No games found for team", team_name, "\n")
  }
  
  # Calculate runs per game
  runs_per_game <- if (games_played > 0) round(total_runs / games_played, 2) else 0
  cat("Runs per game calculation:", total_runs, "/", games_played, "=", runs_per_game, "\n")
  
  # Get team rankings with error handling (this is used for reference but might be needed in future extensions)
  cat("Getting team rankings...\n")
  team_rankings <- tryCatch({
    if (exists("get_team_rankings", mode = "function")) {
      get_team_rankings(team_id_numeric, team_info$DivisionID)
    } else {
      warning("get_team_rankings function not found, using fallback")
      list()
    }
  }, error = function(e) {
    warning(paste("Error getting team rankings:", e$message))
    list()
  })  # nolint
  cat("Team rankings retrieved\n")
  
  # Get team leaders with error handling
  cat("Getting team leaders...\n")
  top_players <- tryCatch({
    if (exists("get_team_leaders", mode = "function")) {
      get_team_leaders(team_id_numeric)
    } else {
      warning("get_team_leaders function not found, using fallback")
      list()
    }
  }, error = function(e) {
    warning(paste("Error getting team leaders:", e$message))
    list()
  })
  cat("Team leaders retrieved\n")
  
  # Calculate team record
  wins <- 0
  losses <- 0
  
  if (!is.null(team_games) && nrow(team_games) > 0) {
    for (i in seq_len(nrow(team_games))) {
      game <- team_games[i,]
      
      # Skip incomplete games
      if (!isTRUE(game$IsComplete)) next
      
      # Make sure we have valid data for comparison
      home_id <- as.numeric(game$HomeTeamID)
      away_id <- as.numeric(game$AwayTeamID)
      home_score <- as.numeric(game$HomeScore)
      away_score <- as.numeric(game$AwayScore)
      
      # Skip any rows with NA values
      if (any(is.na(c(home_id, away_id, home_score, away_score)))) {
        next
      }
      
      if (home_id == team_id_numeric) {
        if (home_score > away_score) wins <- wins + 1
        else losses <- losses + 1
      } else if (away_id == team_id_numeric) {
        if (away_score > home_score) wins <- wins + 1
        else losses <- losses + 1
      }
    }
  }
  
  cat("Team record calculated:", wins, "-", losses, "\n")
  
  # Format statistics for display
  display_stats <- list(
    avg_display = format_avg_stat(batting_stats$AVG),
    obp_display = format_avg_stat(batting_stats$OBP),
    slg_display = format_avg_stat(batting_stats$SLG),
    ops_display = format_avg_stat(batting_stats$OPS),
    ops_plus_display = as.character(round(batting_stats$OPS_plus)),
    era_display = sprintf("%.2f", pitching_stats$ERA),
    era_plus_display = as.character(round(pitching_stats$ERA_plus)),
    whip_display = sprintf("%.2f", pitching_stats$WHIP),
    whip_plus_display = as.character(round(pitching_stats$WHIP_plus)),
    fip_display = sprintf("%.2f", pitching_stats$FIP),
    k9_display = sprintf("%.1f", pitching_stats$K_per_9)
  )
  
  # Create the team snapshot UI with inline CSS to ensure proper rendering
  cat("Building team snapshot UI with inline CSS...\n")
  snapshot_ui <- div(
    class = paste("team-snapshot", team_css_class),
    
    # Team Header with Logo
    div(
      class = "team-snapshot-header",
      tags$img(src = logo_filename, 
               alt = paste(team_name, "Logo"),
               class = "team-snapshot-logo"),
      div(
        class = "team-snapshot-name-record",
        h2(team_name),
        h3(sprintf("Record: %d-%d", wins, losses))
      )
    ),
    
    # Team Statistics
    div(
      class = "row",
      
      # Batting Statistics
      div(
        class = "col-md-6",
        div(
          class = "team-snapshot-stats",
          h3("Batting Statistics"),
          div(
            class = "team-snapshot-stat-item",
            span("Runs Scored/Game:"),
            span(class = "team-snapshot-stat-value", runs_per_game)
          ),
          div(
            class = "team-snapshot-stat-item",
            style = "margin-bottom: 8px; display: flex; justify-content: space-between;",
            span("Team Batting Average:"),
            span(class = "team-snapshot-stat-value", style = "font-weight: bold;", display_stats$avg_display)
          ),
          div(
            class = "team-snapshot-stat-item",
            style = "margin-bottom: 8px; display: flex; justify-content: space-between;",
            span("Team OBP:"),
            span(class = "team-snapshot-stat-value", style = "font-weight: bold;", display_stats$obp_display)
          ),
          div(
            class = "team-snapshot-stat-item",
            style = "margin-bottom: 8px; display: flex; justify-content: space-between;",
            span("Team OPS:"),
            span(class = "team-snapshot-stat-value", style = "font-weight: bold;", display_stats$ops_display)
          ),
          div(
            class = "team-snapshot-stat-item",
            style = "margin-bottom: 8px; display: flex; justify-content: space-between;",
            span("Team OPS+:"),
            span(class = "team-snapshot-stat-value", style = "font-weight: bold;", display_stats$ops_plus_display)
          ),
          div(
            class = "team-snapshot-stat-item",
            style = "margin-bottom: 8px; display: flex; justify-content: space-between;",
            span("SB per Game:"),
            span(class = "team-snapshot-stat-value", style = "font-weight: bold;", 
                 sprintf("%.2f", batting_stats$SB / max(games_played, 1)))
          )
        )
      ),
      
      # Pitching Statistics
      div(
        class = "col-md-6",
        div(
          class = "team-snapshot-stats",
          h3("Pitching Statistics"),
          div(
            class = "team-snapshot-stat-item",
            style = "margin-bottom: 8px; display: flex; justify-content: space-between;",
            span("Team ERA:"),
            span(class = "team-snapshot-stat-value", style = "font-weight: bold;", display_stats$era_display)
          ),
          div(
            class = "team-snapshot-stat-item",
            style = "margin-bottom: 8px; display: flex; justify-content: space-between;",
            span("Team ERA+:"),
            span(class = "team-snapshot-stat-value", style = "font-weight: bold;", display_stats$era_plus_display)
          ),
          div(
            class = "team-snapshot-stat-item",
            style = "margin-bottom: 8px; display: flex; justify-content: space-between;",
            span("Team WHIP:"),
            span(class = "team-snapshot-stat-value", style = "font-weight: bold;", display_stats$whip_display)
          ),
          div(
            class = "team-snapshot-stat-item",
            style = "margin-bottom: 8px; display: flex; justify-content: space-between;",
            span("Team WHIP+:"),
            span(class = "team-snapshot-stat-value", style = "font-weight: bold;", display_stats$whip_plus_display)
          ),
          div(
            class = "team-snapshot-stat-item",
            style = "margin-bottom: 8px; display: flex; justify-content: space-between;",
            span("Team FIP:"),
            span(class = "team-snapshot-stat-value", style = "font-weight: bold;", display_stats$fip_display)
          ),
          div(
            class = "team-snapshot-stat-item",
            style = "margin-bottom: 8px; display: flex; justify-content: space-between;",
            span("Team K/9:"),
            span(class = "team-snapshot-stat-value", style = "font-weight: bold;", display_stats$k9_display)
          )
        )
      )
    ),
    
    # Team Leaders Section
    div(
      class = "team-snapshot-roster",
      h3("Team Leaders"),
      
      # Top Batting Average
      div(
        class = "mb-15",
        style = "margin-bottom: 15px;",
        h4("Batting Average", style = "margin-top: 0; margin-bottom: 10px; font-weight: bold;"),
        if (!is.null(top_players$AVG) && nrow(top_players$AVG) > 0) {
          div(
            class = "team-snapshot-player-list",
            style = "border-left: 3px solid #ddd; padding-left: 10px;",
            lapply(seq_len(min(3, nrow(top_players$AVG))), function(i) {
              div(
                class = "team-snapshot-player-item",
                sprintf("%s: %s", 
                        top_players$AVG$DisplayName[i], 
                        format_avg_stat(top_players$AVG$AVG[i]))
              )
            })
          )
        } else {
          p("No qualified players", style = "font-style: italic; color: #666;")
        },
        
        # Top OPS+
        h4("OPS+", style = "margin-top: 15px; margin-bottom: 10px; font-weight: bold;"),
        if (!is.null(top_players$OPS_Plus) && nrow(top_players$OPS_Plus) > 0) {
          div(
            class = "team-snapshot-player-list",
            style = "border-left: 3px solid #ddd; padding-left: 10px;",
            lapply(seq_len(min(3, nrow(top_players$OPS_Plus))), function(i) {
              div(
                class = "team-snapshot-player-item",
                sprintf("%s: %d", 
                        top_players$OPS_Plus$DisplayName[i],
                        round(top_players$OPS_Plus$OPS_Plus[i]))
              )
            })
          )
        } else {
          p("No qualified players", style = "font-style: italic; color: #666;")
        },
        
        # Top ERA+
        h4("ERA+", style = "margin-top: 15px; margin-bottom: 10px; font-weight: bold;"),
        if (!is.null(top_players$ERA_Plus) && nrow(top_players$ERA_Plus) > 0) {
          div(
            class = "team-snapshot-player-list",
            style = "border-left: 3px solid #ddd; padding-left: 10px;",
            lapply(seq_len(min(3, nrow(top_players$ERA_Plus))), function(i) {
              div(
                class = "team-snapshot-player-item",
                sprintf("%s: %d", 
                        top_players$ERA_Plus$DisplayName[i],
                        round(top_players$ERA_Plus$ERA_Plus[i]))
              )
            })
          )
        } else {
          p("No qualified players", style = "font-style: italic; color: #666;")
        },
        
        # Top sbWAR
        h4("Season Batting WAR (162-game equivalent)", 
           style = "margin-top: 15px; margin-bottom: 10px; font-weight: bold;"),
        if (!is.null(top_players$sbWAR) && nrow(top_players$sbWAR) > 0) {
          div(
            class = "team-snapshot-player-list",
            style = "border-left: 3px solid #ddd; padding-left: 10px;",
            lapply(seq_len(min(3, nrow(top_players$sbWAR))), function(i) {
              div(
                class = "team-snapshot-player-item",
                sprintf("%s: %.1f", 
                        top_players$sbWAR$DisplayName[i],
                        round(top_players$sbWAR$sbWAR[i], 1))
              )
            })
          )
        } else {
          p("No qualified players", style = "font-style: italic; color: #666;")
        },
        
        # Top spWAR
        h4("Season Pitching WAR (162-game equivalent)",
           style = "margin-top: 15px; margin-bottom: 10px; font-weight: bold;"),
        if (!is.null(top_players$spWAR) && nrow(top_players$spWAR) > 0) {
          div(
            class = "team-snapshot-player-list",
            style = "border-left: 3px solid #ddd; padding-left: 10px;",
            lapply(seq_len(min(3, nrow(top_players$spWAR))), function(i) {
              div(
                class = "team-snapshot-player-item",
                sprintf("%s: %.1f", 
                        top_players$spWAR$DisplayName[i],
                        round(top_players$spWAR$spWAR[i], 1))
              )
            })
          )
        } else {
          p("No qualified players", style = "font-style: italic; color: #666;")
        }
      )
    )
  )
  
  cat("Team snapshot UI built successfully\n")
  
  # Wrap the snapshot UI with proper CSS styling
  wrapped_ui <- wrap_team_snapshot(snapshot_ui, team_name)
  cat("Team snapshot UI wrapped with CSS styling\n")
  return(wrapped_ui)
}

# Helper function to wrap team snapshot with consistent CSS styling
wrap_team_snapshot <- function(snapshot_ui, team_name) {
  # Add CSS class for team-specific styling
  team_css_class <- format_team_css_class(team_name)
  
  return(div(
    class = paste("team-snapshot-wrapper", team_css_class),
    style = "max-width: 100%; margin: 0 auto;",
    snapshot_ui
  ))
}