# Player Card Module for TRCStatR
# This module provides functionality for displaying detailed player statistics
# 
# This module expects data from object_engine.R in the following structure:
# - player_stats: A list containing batting and pitching statistics
#   - batting: Data frame with columns including X1B, X2B, X3B, HR, PA, BB, HBP, SF, SH, RBI, K
#   - pitching: Data frame with columns including OutsRecorded, H, R, ER, BB, K, HR, HBP, BF

# Load required libraries
library(shiny)
library(dplyr)
library(htmltools)  # For UI functions like div, h4, tags, etc.

# Declare global variables to avoid lint warnings
utils::globalVariables(c(
  "teams", "players", "get_player_stats", "team_colors", "stat_explanations",
  "batting", "pitching", "TeamID", "TeamName", "FirstName", "LastInitial", "PlayerID",
  "PlayerName", "GameID", "X1B", "X2B", "X3B", "HR", "PA", "BB", "HBP", "SF", "SH",
  "RBI", "K", "AVG", "OBP", "SLG", "OPS", "IP", "ERA", "WHIP", "BF", "OutsRecorded"
))

# UI function for the player card module
player_card_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "player-card-module",
      
      # Team and player selection
      fluidRow(
        column(6,
               selectInput(ns("team_select"), "Select Team:", choices = c("Select a team" = ""))
        ),
        column(6,
               selectInput(ns("player_select"), "Select Player:", choices = c("Select a player" = ""))
        )
      ),
      
      # Generate button
      fluidRow(
        column(12,
               actionButton(ns("generate_card"), "Generate Player Card", 
                            class = "btn-primary", 
                            style = "margin-bottom: 20px;")
        )
      ),
      
      # Card display area
      uiOutput(ns("card_display"))
    )
  )
}

# Server function for the player card module
player_card_server <- function(id, get_teams = NULL, get_players = NULL, get_player_stats = NULL, 
                               get_team_colors = NULL, stat_explanations = NULL) {
  moduleServer(id, function(input, output, session) {
    # Use session$ns for namespacing UI elements
    
    # Use provided functions or fall back to global environment if needed
    get_teams_fn <- get_teams
    get_players_fn <- get_players
    get_player_stats_fn <- get_player_stats
    get_team_colors_fn <- get_team_colors
    
    if (is.null(get_teams_fn)) {
      get_teams_fn <- function() {
        if (exists("teams", envir = .GlobalEnv)) {
          return(get("teams", envir = .GlobalEnv))
        }
        return(NULL)
      }
    }
    
    if (is.null(get_players_fn)) {
      get_players_fn <- function() {
        if (exists("players", envir = .GlobalEnv)) {
          return(get("players", envir = .GlobalEnv))
        }
        return(NULL)
      }
    }
    
    if (is.null(get_player_stats_fn)) {
      get_player_stats_fn <- function(player_id) {
        # First check if master tables exist and use them
        if (exists("master_batting_stats", envir = .GlobalEnv) && 
            exists("master_pitching_stats", envir = .GlobalEnv) && 
            exists("master_war_stats", envir = .GlobalEnv)) {
          
          # Get all master tables
          master_batting <- get("master_batting_stats", envir = .GlobalEnv)
          master_pitching <- get("master_pitching_stats", envir = .GlobalEnv)
          master_war <- get("master_war_stats", envir = .GlobalEnv)
          
          # Filter for this player
          player_batting <- master_batting[master_batting$PlayerID == player_id, ]
          player_pitching <- master_pitching[master_pitching$PlayerID == player_id, ]
          player_war <- master_war[master_war$PlayerID == player_id, ]
          
          # Create a list with all the stats
          player_stats <- list()
          
          # Add batting stats if available
          if (nrow(player_batting) > 0) {
            # Convert to list format expected by the module
            for (col in names(player_batting)) {
              if (col != "PlayerID" && col != "Player") {
                player_stats[[col]] <- player_batting[[col]][1]
              }
            }
          }
          
          # Add pitching stats if available
          if (nrow(player_pitching) > 0) {
            # Convert to list format expected by the module
            for (col in names(player_pitching)) {
              if (col != "PlayerID" && col != "Player") {
                player_stats[[col]] <- player_pitching[[col]][1]
              }
            }
          }
          
          # Add WAR stats if available
          if (nrow(player_war) > 0) {
            # Convert to list format expected by the module
            for (col in names(player_war)) {
              if (col != "PlayerID" && col != "Player") {
                player_stats[[col]] <- player_war[[col]][1]
              }
            }
          }
          
          return(player_stats)
        } else if (exists("get_player_stats", envir = .GlobalEnv)) {
          # Fallback to original function if master tables don't exist
          return(get("get_player_stats", envir = .GlobalEnv)(player_id))
        }
        return(NULL)
      }
    }
    
    if (is.null(get_team_colors_fn)) {
      get_team_colors_fn <- function() {
        if (exists("team_colors", envir = .GlobalEnv)) {
          return(get("team_colors", envir = .GlobalEnv))
        }
        return(NULL)
      }
    }
    
    if (is.null(stat_explanations)) {
      if (exists("stat_explanations", envir = .GlobalEnv)) {
        stat_explanations <- get("stat_explanations", envir = .GlobalEnv)
      } else {
        stat_explanations <- list()
      }
    }
    
    # Reactive value to store selected player info
    player_info <- reactiveVal(NULL)
    
    # Get list of teams on load
    observe({
      teams_data <- get_teams_fn()
      
      if (is.data.frame(teams_data) && nrow(teams_data) > 0) {
        teams_list <- setNames(teams_data$TeamID, teams_data$TeamName)
        updateSelectInput(session, "team_select", choices = c("Select a team" = "", teams_list))
      }
    })
    
    # Update player list when team is selected
    observeEvent(input$team_select, {
      req(input$team_select != "")
      players_data <- get_players_fn()
      
      if (is.data.frame(players_data) && nrow(players_data) > 0) {
        # Filter players by team
        team_players <- players_data[players_data$TeamID == input$team_select, ]
        
        if (nrow(team_players) > 0) {
          # Create player names
          team_players$PlayerName <- paste(team_players$FirstName, team_players$LastInitial)
          player_list <- setNames(team_players$PlayerID, team_players$PlayerName)
          updateSelectInput(session, "player_select", choices = c("Select a player" = "", player_list))
        } else {
          updateSelectInput(session, "player_select", choices = c("No players found" = ""))
        }
      }
    })
    
    # Generate player card when button is clicked
    observeEvent(input$generate_card, {
      req(input$player_select != "")
      
      # Get data needed for player card
      teams_data <- get_teams_fn()
      players_data <- get_players_fn()
      
      # Generate the player card
      output$card_display <- renderUI({
        withProgress(message = 'Generating player card...', value = 0, {
          incProgress(0.3, detail = "Loading player data...")
          
          card <- generate_player_card(
            player_id = input$player_select,
            players = players_data,
            teams = teams_data,
            get_player_stats_fn = get_player_stats_fn,
            get_team_colors_fn = get_team_colors_fn,
            stat_explanations = stat_explanations,
            player_info_rv = player_info
          )
          
          incProgress(0.7)
          card
        })
      })
    })
  })
}

# Format batting averages properly - helper function
# This function formats batting average statistics with proper decimal places
# and handles NA/NULL values appropriately
# @param x The numeric value to format
# @return A formatted string representation of the statistic
format_avg_stat <- function(x) {
  # Check if format_stat exists in object_engine.R
  if (exists("format_stat", envir = .GlobalEnv)) {
    format_fn <- get("format_stat", envir = .GlobalEnv)
    return(format_fn(x, "AVG"))
  }
  
  # Fallback implementation if format_stat is not available
  if (is.na(x) || is.null(x)) return("---")
  sprintf("%.3f", x)
}

# Main player card generation function
generate_player_card <- function(player_id, players, teams, get_player_stats_fn, 
                                 get_team_colors_fn, stat_explanations = list(), player_info_rv = NULL) {
  # Function that gets the opponent name for a game
  get_opponent_name <- function(game_id, team_id) {
    # Read games directly from the provided functions
    if (exists("games", envir = .GlobalEnv)) {
      games_data <- get("games", envir = .GlobalEnv)
      
      game_row <- games_data[games_data$GameID == game_id, ]
      if (nrow(game_row) == 0) return("Unknown")
      
      # Find opponent team ID
      opponent_id <- ifelse(game_row$HomeTeamID == team_id, game_row$AwayTeamID, game_row$HomeTeamID)
      
      # Get opponent name
      team_row <- teams[teams$TeamID == opponent_id, ]
      if (nrow(team_row) == 0) return("Unknown")
      
      return(team_row$TeamName[1])
    }
    
    return("Unknown")
  }
  
  # Main tryCatch block for the function
  result <- tryCatch({
    # Validate inputs to prevent errors
    if (is.null(player_id) || length(player_id) == 0) {
      return(div(class = "alert alert-danger", "Error: Invalid player ID"))
    }
    
    # Convert player_id to numeric
    player_id_numeric <- as.numeric(as.character(player_id))
    
    # Get player info
    if (length(player_id_numeric) == 1 && !is.na(player_id_numeric)) {
      player_info <- players[players$PlayerID == player_id_numeric, ]
      if (nrow(player_info) > 0) {
        # Take just the first row if multiple matches
        player_info <- player_info[1, ]
      }
    } else {
      player_info <- data.frame()
    }
    
    # Check if player exists
    if (nrow(player_info) == 0) {
      return(div(class = "alert alert-warning", "Player not found. Please select a valid player."))
    }
    
    # Store player_info in the reactive value if provided
    if (!is.null(player_info_rv) && is.function(player_info_rv)) {
      player_info_rv(player_info)
    }
    
    # Get player stats using the provided function
    player_stats <- get_player_stats_fn(player_id_numeric)
    
    # Get team info
    team_id <- player_info$TeamID
    team_name <- "Unknown Team"
    
    if (!is.null(team_id) && length(team_id) == 1 && !is.na(team_id)) {
      team_info <- teams[teams$TeamID == team_id, ]
      if (nrow(team_info) > 0) {
        team_name <- team_info$TeamName[1]
      }
    }
    
    # Get team colors
    team_colors_list <- get_team_colors_fn()
    if (!is.null(team_colors_list) && team_name %in% names(team_colors_list)) {
      player_info$PrimaryColor <- team_colors_list[[team_name]]$primary
      player_info$SecondaryColor <- team_colors_list[[team_name]]$secondary
      player_info$TertiaryColor <- team_colors_list[[team_name]]$tertiary
    } else {
      # Default colors
      player_info$PrimaryColor <- "#800000"
      player_info$SecondaryColor <- "#AB4E52"
      player_info$TertiaryColor <- "#FFFFFF"
    }
    
    # Get batting and pitching data for game logs
    batting_data <- NULL
    pitching_data <- NULL
    
    if (exists("batting", envir = .GlobalEnv)) {
      batting_data <- get("batting", envir = .GlobalEnv)
      batting_data <- batting_data[batting_data$PlayerID == player_id_numeric, ]
      
      if (nrow(batting_data) > 0) {
        batting_data$Opponent <- sapply(
          batting_data$GameID, 
          function(gid) get_opponent_name(gid, team_id)
        )
        batting_data <- batting_data[order(batting_data$GameID), ]
      }
    }
    
    if (exists("pitching", envir = .GlobalEnv)) {
      pitching_data <- get("pitching", envir = .GlobalEnv)
      pitching_data <- pitching_data[pitching_data$PlayerID == player_id_numeric, ]
      
      if (nrow(pitching_data) > 0) {
        pitching_data$Opponent <- sapply(
          pitching_data$GameID, 
          function(gid) get_opponent_name(gid, team_id)
        )
        pitching_data <- pitching_data[order(pitching_data$GameID), ]
      }
    }
    
    # Add game logs to player_stats
    player_stats$batting <- batting_data
    player_stats$pitching <- pitching_data
    
    # Create batting and pitching game log UI elements
    # Define these before the main UI creation to avoid duplication
    batting_log_ui <- NULL
    if (!is.null(player_stats$batting) && is.data.frame(player_stats$batting) && nrow(player_stats$batting) > 0) {
      batting_log_ui <- div(
        class = "batting-gamelog-section",
        style = paste0(
          "margin: 30px auto; ",
          "padding: 20px; ",
          "border: 2px solid ", player_info$PrimaryColor, "; ",
          "border-radius: 10px; ",
          "background-color: #FFFFFF ; ",
          "max-width: 95%;"
        ),
        
        h4(
          "Batting Game Log",
          style = paste0(
            "font-size: 1.2em; ",
            "font-weight: bold; ",
            "color: ", player_info$SecondaryColor, "; ",
            "margin-top: 0; ",
            "margin-bottom: 10px; ",
            "font-family: 'Freshman', sans-serif; ",
            "text-align: center;"
          )
        ),
        
        # Game log table
        div(
          class = "batting-gamelog-table",
          style = "width: 100%; overflow-x: auto;",
          
          tags$table(
            class = "table table-striped table-bordered",
            style = "width: 100%; font-size: 0.9em;",
            
            # Table header
            tags$thead(
              tags$tr(
                style = paste0("background-color: ", player_info$PrimaryColor, "; color: white; font-weight: bold;"),
                tags$th("Opponent"),
                tags$th("PA"),
                tags$th("AB"),
                tags$th("H"),
                tags$th("HR"),
                tags$th("RBI"),
                tags$th("BB"),
                tags$th("K"),
                tags$th("AVG"),
                tags$th("OBP")
              )
            ),
            
            # Table body
            tags$tbody(
              lapply(seq_len(nrow(player_stats$batting)), function(i) {
                row_data <- player_stats$batting[i, ]
                
                # Calculate stats using safe functions from object_engine.R
                safe_batting_avg <- if(exists("safe_batting_avg", envir = .GlobalEnv)) {
                  get("safe_batting_avg", envir = .GlobalEnv)
                } else {
                  function(h, ab) { if(ab > 0) h/ab else 0 }
                }
                
                safe_obp <- if(exists("safe_obp", envir = .GlobalEnv)) {
                  get("safe_obp", envir = .GlobalEnv)
                } else {
                  function(h, bb, hbp, ab, sf) {
                    if(ab + bb + hbp + sf > 0) (h + bb + hbp)/(ab + bb + hbp + sf) else 0
                  }
                }
                
                # Calculate derived stats
                hits <- row_data$X1B + row_data$X2B + row_data$X3B + row_data$HR
                at_bats <- row_data$PA - row_data$BB - row_data$HBP - row_data$SF - row_data$SH
                
                # Use safe functions
                avg <- safe_batting_avg(hits, at_bats)
                obp <- safe_obp(hits, row_data$BB, row_data$HBP, at_bats, row_data$SF)
                
                tags$tr(
                  tags$td(row_data$Opponent),
                  tags$td(row_data$PA),
                  tags$td(at_bats),
                  tags$td(hits),
                  tags$td(row_data$HR),
                  tags$td(row_data$RBI),
                  tags$td(row_data$BB),
                  tags$td(row_data$K),
                  tags$td(format_avg_stat(avg)),
                  tags$td(format_avg_stat(obp))
                )
              })
            )
          )
        )
      )
    }
    
    # Pitching Game Log section
    pitching_log_ui <- NULL
    if (!is.null(player_stats$pitching) && is.data.frame(player_stats$pitching) && nrow(player_stats$pitching) > 0) {
      pitching_log_ui <- div(
        class = "pitching-gamelog-section",
        style = paste0(
          "margin: 30px auto; ",
          "padding: 20px; ",
          "border: 2px solid ", player_info$PrimaryColor, "; ",
          "border-radius: 10px; ",
          "background-color: #FFFFFF ; ",
          "max-width: 95%;"
        ),
        
        h4(
          "Pitching Game Log",
          style = paste0(
            "font-size: 1.2em; ",
            "font-weight: bold; ",
            "color: ", player_info$SecondaryColor, "; ",
            "margin-top: 0; ",
            "margin-bottom: 10px; ",
            "font-family: 'Freshman', sans-serif; ",
            "text-align: center;"
          )
        ),
        
        # Game log table
        div(
          class = "pitching-gamelog-table",
          style = "width: 100%; overflow-x: auto;",
          
          tags$table(
            class = "table table-striped table-bordered",
            style = "width: 100%; font-size: 0.9em;",
            
            # Table header
            tags$thead(
              tags$tr(
                style = paste0("background-color: ", player_info$PrimaryColor, "; color: white; font-weight: bold;"),
                tags$th("Opponent"),
                tags$th("IP"),
                tags$th("H"),
                tags$th("R"),
                tags$th("ER"),
                tags$th("BB"),
                tags$th("K"),
                tags$th("HR"),
                tags$th("HBP")
              )
            ),
            
            # Table body
            tags$tbody(
              lapply(seq_len(nrow(player_stats$pitching)), function(i) {
                row_data <- player_stats$pitching[i, ]
                
                # Get safe functions from object_engine.R if available
                safe_innings_pitched <- if(exists("safe_innings_pitched", envir = .GlobalEnv)) {
                  get("safe_innings_pitched", envir = .GlobalEnv)
                } else {
                  function(outs) {
                    ip <- outs / 3
                    floor(ip) + (outs %% 3) / 10
                  }
                }
                
                # Calculate stats using safe functions
                ip_display <- safe_innings_pitched(row_data$OutsRecorded)
                
                tags$tr(
                  tags$td(row_data$Opponent),
                  tags$td(sprintf("%.1f", ip_display)),
                  tags$td(row_data$H),
                  tags$td(row_data$R),
                  tags$td(row_data$ER),
                  tags$td(row_data$BB),
                  tags$td(row_data$K),
                  tags$td(row_data$HR),
                  tags$td(row_data$HBP)
                )
              })
            )
          )
        )
      )
    }
    
    # Create the player card HTML
    card_html <- div(
      class = "player-card",
      style = paste0(
        "border: 3px solid ", player_info$PrimaryColor, "; ",
        "border-radius: 10px; ",
        "padding: 20px; ",
        "background: white; ",
        "box-shadow: 0 4px 12px rgba(0,0,0,0.1)"
      ),
      
      # Script for tooltips
      tags$script(HTML("$(document).ready(function(){ $('.stat-tooltip').tooltip(); });")),
      
      # Header section with team logo and player info
      div(
        class = "player-header",
        style = paste0(
          "display: flex; ",
          "align-items: center; ",
          "margin-bottom: 20px; ",
          "padding-bottom: 15px; ",
          "border-bottom: 2px solid ", player_info$PrimaryColor, ";"
        ),
        
        # Team logo
        div(
          class = "team-logo",
          style = "margin-right: 20px;",
          tags$img(
            # In Shiny, files in www directory are served directly without the www/ prefix
            src = paste0(gsub(" ", "_", tolower(team_name)), ".png"),
            style = "width: 80px; height: 80px; object-fit: contain;",
            # Add onerror handler to show a default image if the team logo fails to load
            onerror = "this.onerror=null; this.src='default_team.png';"
          )
        ),
        
        # Player info
        div(
          class = "player-info",
          style = "flex-grow: 1;",
          
          # Player name
          div(
            class = "player-name",
            style = paste0(
              "font-size: 2.2em; ",
              "font-weight: bold; ",
              "color: ", player_info$PrimaryColor, "; ",
              "margin-bottom: 5px; ",
              "font-family: 'Freshman', sans-serif;"
            ),
            paste0(
              ifelse(is.null(player_info$FirstName) || length(player_info$FirstName) == 0, "", player_info$FirstName),
              " ",
              if(!is.null(player_info$LastName) && length(player_info$LastName) == 1 && !is.na(player_info$LastName)) {
                player_info$LastName
              } else if(!is.null(player_info$LastInitial) && length(player_info$LastInitial) == 1 && !is.na(player_info$LastInitial)) {
                player_info$LastInitial
              } else {
                ""
              }
            )
          ),
          
          # Team name
          div(
            class = "team-name",
            style = paste0(
              "font-size: 1.2em; ",
              "color: ", player_info$SecondaryColor, "; ",
              "margin-bottom: 5px; ",
              "font-family: 'Freshman', sans-serif;"
            ),
            team_name
          ),
          
          # Jersey number
          div(
            class = "player-details",
            style = paste0(
              "font-size: 1.8em; ",
              "font-weight: bold; ",
              "color: ", player_info$SecondaryColor, "; ",
              "font-family: 'Freshman', sans-serif;"
            ),
            paste0("#", ifelse(is.null(player_info$JerseyNumber) || length(player_info$JerseyNumber) == 0, "", player_info$JerseyNumber))
          )
        )
      ),
      
      # Stats section
      div(
        class = "stats-section",
        style = "margin-top: 20px;",
        
        if (!is.null(player_stats)) {
          tagList(
            # Season header
            h3(
              "Season Statistics",
              style = paste0("color: ", player_info$PrimaryColor, "; margin-top: 0; margin-bottom: 15px;")
            ),
            
            # Batting stats section
            div(
              class = "batting-stats-section",
              style = "margin-bottom: 25px;",
              
              h4(
                "Batting",
                style = paste0("color: ", player_info$SecondaryColor, "; margin-top: 0; margin-bottom: 10px;")
              ),
              
              # Batting stats grid
              div(
                class = "stats-grid",
                style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(120px, 1fr)); gap: 15px;",
                
                # Generate stat boxes for specified batting stats
                lapply(c("AVG", "H", "HR", "OBP", "SLG", "OPS", "OPS_plus", "BB", "K", "SB", 
                         "sbWAR", "gWAR"), function(stat_name) {
                  if (!stat_name %in% names(player_stats)) {
                    return(NULL)
                  }
                  
                  # Get the display name for the stat
                  display_name <- gsub("_", " ", stat_name)
                  
                  # Format the stat value
                  stat_value <- player_stats[[stat_name]]
                  
                  formatted_value <- if (is.numeric(stat_value) && length(stat_value) == 1) {
                    if (any(grepl("sbWAR|spWAR|gWAR|wTWAR", stat_name))) {
                      sprintf("%.1f", stat_value)
                    } else if (any(grepl("SLG|OPS|OBP", stat_name)) && !any(grepl("plus|Plus", stat_name))) {
                      sprintf("%.3f", stat_value)
                    } else if (any(grepl("AVG|wOBA|wBC", stat_name))) {
                      format_avg_stat(stat_value)
                    } else if (any(grepl("ERA", stat_name)) && !any(grepl("plus|Plus", stat_name))) {
                      sprintf("%.2f", stat_value)
                    } else if (any(grepl("IP", stat_name))) {
                      sprintf("%.1f", stat_value)
                    } else if (any(grepl("plus|Plus", stat_name))) {
                      sprintf("%d", round(stat_value))
                    } else if (any(grepl("K_BB|K/9|BB/9", stat_name))) {
                      sprintf("%.1f", stat_value)
                    } else {
                      sprintf("%d", round(stat_value))
                    }
                  } else if (is.numeric(stat_value) && length(stat_value) > 1) {
                    # Handle vector values (use first element)
                    if (any(grepl("sbWAR|spWAR|gWAR|wTWAR", stat_name))) {
                      sprintf("%.1f", stat_value[1])
                    } else if (any(grepl("SLG|OPS", stat_name)) && !any(grepl("plus|Plus", stat_name))) {
                      sprintf("%.3f", stat_value[1])
                    } else if (any(grepl("AVG|OBP|wOBA|wBC", stat_name))) {
                      format_avg_stat(stat_value[1])
                    } else if (any(grepl("ERA", stat_name)) && !any(grepl("plus|Plus", stat_name))) {
                      sprintf("%.2f", stat_value[1])
                    } else if (any(grepl("IP", stat_name))) {
                      sprintf("%.1f", stat_value[1])
                    } else if (any(grepl("plus|Plus", stat_name))) {
                      sprintf("%d", round(stat_value[1]))
                    } else if (any(grepl("K_BB|K/9|BB/9", stat_name))) {
                      sprintf("%.1f", stat_value[1])
                    } else {
                      sprintf("%d", round(stat_value[1]))
                    }
                  } else {
                    if (is.character(stat_value)) {
                      stat_value
                    } else {
                      "N/A"
                    }
                  }
                  
                  # Create the stat box
                  div(
                    class = "stat-box",
                    style = paste0(
                      "padding: 10px; ",
                      "border: 1px solid ", player_info$PrimaryColor, "; ",
                      "border-radius: 5px; ",
                      "text-align: center; ",
                      "background-color: ", player_info$SecondaryColor, ";"
                    ),
                    
                    # Stat name
                    div(
                      class = "stat-name",
                      style = paste0(
                        "font-size: 0.9em; ",
                        "font-weight: bold; ",
                        "color: ", player_info$TertiaryColor, "; ",
                        "text-shadow: 1px 1px 1px ", player_info$PrimaryColor, "; ",
                        "margin-bottom: 5px;"
                      ),
                      display_name,
                      
                      # Add tooltip if we have an explanation
                      if (length(stat_name) == 1 && !is.na(stat_name) && stat_name %in% names(stat_explanations)) {
                        tags$span(
                          class = "stat-tooltip",
                          style = "margin-left: 5px; cursor: help;",
                          `data-toggle` = "tooltip",
                          `data-placement` = "top",
                          title = stat_explanations[[stat_name]],
                          icon("question-circle", lib = "font-awesome")
                        )
                      } else {
                        NULL
                      }
                    ),
                    
                    # Stat value
                    div(
                      class = "stat-value",
                      style = paste0(
                        "font-size: 1.5em; ",
                        "font-weight: bold; ",
                        "color: ", player_info$PrimaryColor, ";"
                      ),
                      formatted_value
                    )
                  )
                })
              )
            ),
            
            # Include the batting game log UI that we defined earlier
            batting_log_ui,
            
            # Central wTWAR container
            if ("wTWAR" %in% names(player_stats)) {
              div(
                class = "central-war-container",
                style = paste0(
                  "margin: 30px auto; ",
                  "padding: 20px; ",
                  "border: 2px solid ", player_info$TertiaryColor, "; ",
                  "border-radius: 10px; ",
                  "background-color: ", player_info$PrimaryColor, ";",
                  "text-align: center; ",
                  "max-width: 500px;"
                ),
                
                div(
                  class = "war-title",
                  style = paste0(
                    "font-size: 1.2em; ",
                    "font-weight: bold; ",
                    "color: ", player_info$SecondaryColor, "; ",
                    "margin-bottom: 10px;"
                  ),
                  "Weighted Total Wins Above Replacement (wTWAR)"
                ),
                
                div(
                  class = "war-value",
                  style = paste0(
                    "font-size: 2.5em; ",
                    "font-weight: bold; ",
                    "color: ", player_info$TertiaryColor, "; ",
                    "text-shadow: 1px 1px 3px #000000; ",
                    "margin-bottom: 15px;"
                  ),
                  if (is.numeric(player_stats$wTWAR) && length(player_stats$wTWAR) > 0) {
                    sprintf("%.1f", player_stats$wTWAR[1])
                  } else {
                    "N/A"
                  }
                ),
                
                div(
                  class = "war-explanation",
                  style = paste0(
                    "font-size: 0.9em; ",
                    "color: ", player_info$SecondaryColor, "; ",
                    "line-height: 1.4;"
                  ),
                  # Use explanation from stat_explanations if available
                  if ("wTWAR" %in% names(stat_explanations)) {
                    stat_explanations[["wTWAR"]]
                  } else {
                    "wTWAR combines a player's batting and pitching contributions into a single value representing total wins added above a replacement-level player."
                  }
                )
              )
            } else {
              NULL
            },
            
            # Only show pitching stats section if player has pitched (IP > 0)
            if (!is.null(player_stats$IP) && is.numeric(player_stats$IP) && player_stats$IP > 0) {
              div(
                class = "pitching-stats-section",
                style = "margin-top: 25px;",
                
                h4(
                  "Pitching",
                  style = paste0("color: ", player_info$SecondaryColor, "; margin-top: 0; margin-bottom: 10px;")
                ),
                
                # Pitching stats grid
                div(
                  class = "stats-grid",
                  style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(120px, 1fr)); gap: 15px;",
                  
                  # Only show specified pitching stats
                  lapply(c("IP", "ERA", "ERA_plus", "WHIP", "WHIP_plus", "Pitch_K", "K_pct", "FIP", "spWAR"), function(stat_name) {
                    # Get the display name for the stat
                    display_name <- gsub("_", " ", stat_name)
                    if (stat_name == "Pitch_K") display_name <- "K"
                    if (stat_name == "ERA_plus") display_name <- "ERA+"
                    if (stat_name == "WHIP_plus") display_name <- "WHIP+"
                    if (stat_name == "K_pct") display_name <- "K Rate"
                    
                    if (!stat_name %in% names(player_stats)) {
                      return(NULL)
                    }
                    
                    # Format the stat value (same pattern as batting stats)
                    stat_value <- player_stats[[stat_name]]
                    
                    formatted_value <- if (is.numeric(stat_value) && length(stat_value) == 1) {
                      if (any(grepl("sbWAR|spWAR|gWAR|wTWAR", stat_name))) {
                        sprintf("%.1f", stat_value)
                      } else if (stat_name == "WHIP_plus") {
                        sprintf("%d", round(stat_value))
                      } else if (any(grepl("plus|Plus", stat_name))) {
                        sprintf("%d", round(stat_value))
                      } else if (stat_name == "K_pct") {
                        # Format K_pct as a percentage with one decimal place
                        sprintf("%.1f%%", stat_value * 100)
                      } else if (any(grepl("pct|Rate", stat_name)) && stat_name != "K_pct") {
                        format_avg_stat(stat_value)
                      } else if (any(grepl("ERA|FIP", stat_name)) && !any(grepl("plus|Plus", stat_name))) {
                        sprintf("%.2f", stat_value)
                      } else if (stat_name == "WHIP") {
                        sprintf("%.2f", stat_value)
                      } else if (any(grepl("IP", stat_name))) {
                        sprintf("%.1f", stat_value)
                      } else {
                        sprintf("%d", round(stat_value))
                      }
                    } else if (is.numeric(stat_value) && length(stat_value) > 1) {
                      # Handle vector values (same pattern as batting)
                      if (any(grepl("sbWAR|spWAR|gWAR|wTWAR", stat_name))) {
                        sprintf("%.1f", stat_value[1])
                      } else if (stat_name == "WHIP_plus") {
                        sprintf("%d", round(stat_value[1]))
                      } else if (any(grepl("plus|Plus", stat_name))) {
                        sprintf("%d", round(stat_value[1]))
                      } else if (any(grepl("pct|Rate", stat_name))) {
                        format_avg_stat(stat_value[1])
                      } else if (any(grepl("ERA|FIP", stat_name)) && !any(grepl("plus|Plus", stat_name))) {
                        sprintf("%.2f", stat_value[1])
                      } else if (stat_name == "WHIP") {
                        sprintf("%.2f", stat_value[1])
                      } else if (any(grepl("IP", stat_name))) {
                        sprintf("%.1f", stat_value[1])
                      } else {
                        sprintf("%d", round(stat_value[1]))
                      }
                    } else {
                      if (is.character(stat_value)) {
                        stat_value
                      } else {
                        "N/A"
                      }
                    }
                    
                    # Create the stat box (identical pattern to batting stats)
                    div(
                      class = "stat-box",
                      style = paste0(
                        "padding: 10px; ",
                        "border: 1px solid ", player_info$PrimaryColor, "; ",
                        "border-radius: 5px; ",
                        "text-align: center; ",
                        "background-color: ", player_info$SecondaryColor, ";"
                      ),
                      
                      # Stat name
                      div(
                        class = "stat-name",
                        style = paste0(
                          "font-size: 0.9em; ",
                          "font-weight: bold; ",
                          "color: ", player_info$TertiaryColor, "; ",
                          "text-shadow: 1px 1px 1px ", player_info$PrimaryColor, "; ",
                          "margin-bottom: 5px;"
                        ),
                        display_name,
                        
                        # Add tooltip if we have an explanation
                        if (length(stat_name) == 1 && !is.na(stat_name) && stat_name %in% names(stat_explanations)) {
                          tags$span(
                            class = "stat-tooltip",
                            style = "margin-left: 5px; cursor: help;",
                            `data-toggle` = "tooltip",
                            `data-placement` = "top",
                            title = stat_explanations[[stat_name]],
                            icon("question-circle", lib = "font-awesome")
                          )
                        } else {
                          NULL
                        }
                      ),
                      
                      # Stat value
                      div(
                        class = "stat-value",
                        style = paste0(
                          "font-size: 1.5em; ",
                          "font-weight: bold; ",
                          "color: ", player_info$PrimaryColor, ";"
                        ),
                        formatted_value
                      )
                    )
                  })
                )
              )
            } else {
              NULL
            },
            
            # Include the pitching game log UI that we defined earlier
            pitching_log_ui
          )
        } else {
          # If no stats, show a message
          div(
            class = "no-stats-message",
            style = paste0(
              "padding: 20px; ",
              "text-align: center; ",
              "color: ", player_info$SecondaryColor, "; ",
              "font-style: italic;"
            ),
            "No statistics available for this player."
          )
        }
      )
    )
    
    # Return the card_html
    return(card_html)
    
  }, error = function(e) {
    # Log any errors that occur
    message("ERROR in generate_player_card:", e$message)
    
    # Return an error message to display
    div(
      class = "alert alert-danger",
      "Error generating player card. Please try again or contact support."
    )
  })
  
  # Return the result
  return(result)
}
                                                                  
                                                                  