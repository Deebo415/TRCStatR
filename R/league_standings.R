# league_standings.R
# This module displays team standings sorted by winning percentage
# 
# This module expects data from object_engine.R and requires the following functions:
# - get_games(): Returns a data frame of games with HomeTeamID, AwayTeamID, HomeScore, AwayScore, IsComplete
# - get_teams(): Returns a data frame of teams with TeamID, TeamName
# - safe_divide(): A function for safely handling division operations

# Load required packages
library(shiny)
library(shinydashboard)
library(htmltools)

# Declare global variables to avoid lint warnings
utils::globalVariables(c(
  "TeamID", "TeamName", "HomeTeamID", "AwayTeamID", "HomeScore", "AwayScore", "IsComplete",
  "GameID", "Wins", "Losses", "Ties", "RunsScored", "RunsAgainst", "RunDifferential",
  "GamesPlayed", "WinningPct", "LogoFilename", "PCT", "get_team_logo", "safe_divide"
))

# Function to calculate team standings
calculate_standings <- function(get_games, get_teams) {
  # Add debug output
  message("Starting to calculate standings...")
  
  tryCatch({
    # First check if master_team_standings exists in global environment
    if (exists("master_team_standings", envir = .GlobalEnv)) {
      message("Using master_team_standings from global environment")
      
      # Get the master team standings
      master_standings <- get("master_team_standings", envir = .GlobalEnv)
      
      # Get teams to ensure we have team names
      teams <- get_teams()
      
      if (is.null(master_standings) || nrow(master_standings) == 0) {
        message("master_team_standings exists but is empty")
        return(NULL)
      }
      
      # Create a standings data frame with the required columns
      # IMPORTANT: We need to maintain the exact same order as master_standings
      standings <- data.frame(
        TeamID = master_standings$TeamID,
        TeamName = master_standings$Team,
        Wins = master_standings$W,
        Losses = master_standings$L,
        Ties = master_standings$T,
        RunsScored = master_standings$RS,
        RunsAgainst = master_standings$RA,
        RunDifferential = master_standings$RD,
        GamesPlayed = master_standings$W + master_standings$L + master_standings$T,
        WinningPct = master_standings$PCT,
        stringsAsFactors = FALSE
      )
      
      # Ensure we keep the exact same order as master_standings
      # This is critical for maintaining the sorting by PCT, DIFF, RS, RA
      standings <- standings[match(master_standings$TeamID, standings$TeamID),]
      
      # Add logo filename based on team name
      standings$LogoFilename <- sapply(standings$TeamName, function(name) {
        if (is.na(name) || name == "") {
          return("tigers")  # Default to Tigers team logo
        } else {
          # Use the centralized get_team_logo function if available
          if (exists("get_team_logo", envir = .GlobalEnv)) {
            logo_path <- get("get_team_logo", envir = .GlobalEnv)(name)
            # Extract filename without extension or path
            return(gsub("^www/|\\.png$", "", logo_path))
          } else {
            # Fallback to a simple conversion if function not available
            return(gsub(" ", "_", tolower(name)))
          }
        }
      })
      
      # Format winning percentage for display
      standings$PCT <- sprintf("%.3f", standings$WinningPct)
      
      # Return the standings data
      message("Successfully created standings from master_team_standings with ", nrow(standings), " teams")
      return(standings)
    } else {
      message("master_team_standings not found in global environment, calculating from scratch")
      
      # Get all completed games
      games <- get_games()
      teams <- get_teams()
      message("Retrieved ", ifelse(is.null(games), "NULL", nrow(games)), " games")
      message("Retrieved ", ifelse(is.null(teams), "NULL", nrow(teams)), " teams")
      if (is.null(games) || nrow(games) == 0 || is.null(teams) || nrow(teams) == 0) {
        return(NULL)
      }
      
      # Initialize standings data frame
      standings <- data.frame(
        TeamID = teams$TeamID,
        TeamName = teams$TeamName,
        Wins = 0,
        Losses = 0,
        Ties = 0,
        RunsScored = 0,
        RunsAgainst = 0,
        stringsAsFactors = FALSE
      )
      
      # Replace NA values with appropriate defaults
      standings$TeamID[is.na(standings$TeamID)] <- 0
      standings$TeamName[is.na(standings$TeamName)] <- "Unknown Team"
      
      # Add logo filename based on team name
      standings$LogoFilename <- sapply(standings$TeamName, function(name) {
        if (is.na(name) || name == "") {
          return("tigers")  # Default to Tigers team logo
        } else {
          # Use the centralized get_team_logo function if available
          if (exists("get_team_logo", envir = .GlobalEnv)) {
            logo_path <- get("get_team_logo", envir = .GlobalEnv)(name)
            # Extract filename without extension or path
            return(gsub("^www/|\\.png$", "", logo_path))
          } else {
            # Fallback to a simple conversion if function not available
            return(gsub(" ", "_", tolower(name)))
          }
        }
      })
      
      message("Initialized standings data frame with ", nrow(standings), " teams")
      
      # Create a matrix to track head-to-head results
      h2h_matrix <- matrix(0, nrow = nrow(teams), ncol = nrow(teams))
      
      # Convert team IDs to character to ensure proper indexing
      team_ids_char <- as.character(teams$TeamID)
      
      # Check for duplicates in team IDs
      if (length(team_ids_char) != length(unique(team_ids_char))) {
        message("Warning: Duplicate team IDs found")
        team_ids_char <- make.unique(team_ids_char)
      }
      
      rownames(h2h_matrix) <- team_ids_char
      colnames(h2h_matrix) <- team_ids_char
      
      message("Created head-to-head matrix with dimensions: ", nrow(h2h_matrix), "x", ncol(h2h_matrix))
      
      # Process each game to calculate standings
      for (i in seq_len(nrow(games))) {
        home_id <- games$HomeTeamID[i]
        away_id <- games$AwayTeamID[i]
        home_score <- games$HomeScore[i]
        away_score <- games$AwayScore[i]
        
        # Skip games with NA values
        if (is.na(home_id) || is.na(away_id) || is.na(home_score) || is.na(away_score)) {
          message("Skipping game with NA values: GameID=", games$GameID[i])
          next
        }
        
        # Find indices in standings data frame
        home_idx <- which(standings$TeamID == home_id)
        away_idx <- which(standings$TeamID == away_id)
        
        # Skip if team not found
        if (length(home_idx) == 0 || length(away_idx) == 0) {
          message("Team not found in standings: HomeID=", home_id, ", AwayID=", away_id)
          next
        }
        
        # Convert IDs to character for matrix indexing
        home_id_char <- as.character(home_id)
        away_id_char <- as.character(away_id)
        
        # Determine winner
        if (home_score > away_score) {
          # Home team wins
          standings$Wins[home_idx] <- standings$Wins[home_idx] + 1
          standings$Losses[away_idx] <- standings$Losses[away_idx] + 1
          
          # Update head-to-head matrix
          if (home_id_char %in% rownames(h2h_matrix) && away_id_char %in% colnames(h2h_matrix)) {
            h2h_matrix[home_id_char, away_id_char] <- h2h_matrix[home_id_char, away_id_char] + 1
          }
        } else if (away_score > home_score) {
          # Away team wins
          standings$Wins[away_idx] <- standings$Wins[away_idx] + 1
          standings$Losses[home_idx] <- standings$Losses[home_idx] + 1
          
          # Update head-to-head matrix
          if (away_id_char %in% rownames(h2h_matrix) && home_id_char %in% colnames(h2h_matrix)) {
            h2h_matrix[away_id_char, home_id_char] <- h2h_matrix[away_id_char, home_id_char] + 1
          }
        } else {
          # Tie
          standings$Ties[home_idx] <- standings$Ties[home_idx] + 1
          standings$Ties[away_idx] <- standings$Ties[away_idx] + 1
        }
        
        # Update runs scored and against
        standings$RunsScored[home_idx] <- standings$RunsScored[home_idx] + home_score
        standings$RunsAgainst[home_idx] <- standings$RunsAgainst[home_idx] + away_score
        
        standings$RunsScored[away_idx] <- standings$RunsScored[away_idx] + away_score
        standings$RunsAgainst[away_idx] <- standings$RunsAgainst[away_idx] + home_score
      }
      
      # Calculate additional statistics
      standings$RunDifferential <- standings$RunsScored - standings$RunsAgainst
      standings$GamesPlayed <- standings$Wins + standings$Losses + standings$Ties
      
      # Calculate winning percentage using the safe_divide function if available
      if (exists("safe_divide", envir = .GlobalEnv)) {
        safe_div <- get("safe_divide", envir = .GlobalEnv)
        standings$WinningPct <- sapply(seq_len(nrow(standings)), function(i) {
          safe_div(standings$Wins[i] + (0.5 * standings$Ties[i]), standings$GamesPlayed[i])
        })
      } else {
        # Fallback implementation if safe_divide is not available
        standings$WinningPct <- sapply(seq_len(nrow(standings)), function(i) {
          games_played <- standings$GamesPlayed[i]
          if (is.na(games_played) || games_played == 0) return(0)
          (standings$Wins[i] + (0.5 * standings$Ties[i])) / games_played
        })
      }
      
      # Sort by winning percentage, run differential, runs scored, and runs against (decreasing)
      # For runs against, we want ascending order (lower is better), so we negate it when decreasing=TRUE
      standings <- standings[order(
        standings$WinningPct, 
        standings$RunDifferential, 
        standings$RunsScored, 
        -standings$RunsAgainst, 
        decreasing = TRUE
      ), ]
      
      # We're prioritizing the sorting order: PCT, DIFF, RS, RA
      # The head-to-head tiebreaker is disabled to ensure this order is maintained
      # If you want to enable head-to-head tiebreakers in the future, you can add it back
      
      # Format winning percentage for display
      standings$PCT <- sprintf("%.3f", standings$WinningPct)
      
      # Return the standings data
      return(standings)
    }
  }, error = function(e) {
    message("Error calculating standings: ", e$message)
    # Print the call stack for better debugging
    message("Call:", deparse(e$call))
    return(NULL)
  })
}

# UI function for league standings
league_standings_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             div(
               class = "standings-header",
               h1("League Standings", class = "text-center", style = "font-family: 'Freshman', sans-serif; margin-bottom: 30px;")
             )
      )
    ),
    fluidRow(
      column(12,
             div(
               class = "standings-container",
               uiOutput(ns("standings_table"))
             )
      )
    ),
    tags$script(HTML("
      // Add custom styling for run differential
      $(document).ready(function() {
        function styleRunDifferential() {
          $('.run-diff-positive').css('color', 'green');
          $('.run-diff-negative').css('color', 'red');
          $('.run-diff-zero').css('color', 'black');
        }
        
        // Apply initially and whenever Shiny updates the content
        styleRunDifferential();
        $(document).on('shiny:value', function(event) {
          setTimeout(styleRunDifferential, 100);
        });
      });
    "))
  )
}

# Server function for league standings
league_standings_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Reactive to get standings data
    standings_data <- reactive({
      withProgress(message = 'Calculating standings...', value = 0, {
        result <- calculate_standings(data$get_games, data$get_teams)
        incProgress(1)
        result
      })
    })
    
    # Render the standings table
    output$standings_table <- renderUI({
      data <- standings_data()
      
      if (is.null(data) || nrow(data) == 0) {
        return(div(
          class = "alert alert-info",
          "No standings data available. This could be because there are no completed games in the database."
        ))
      }
      
      # CRITICAL FIX: Always ensure proper sorting before rendering
      # This guarantees correct sorting regardless of how the module is initialized
      data <- data[order(
        data$WinningPct,
        data$RunDifferential,
        data$RunsScored,
        -data$RunsAgainst,
        decreasing = TRUE
      ), ]
      
      # Create the standings table
      div(
        class = "standings-table-wrapper",
        style = "background-color: #800000; color: white; padding: 20px; border-radius: 5px;",
        tags$table(
          class = "table table-striped table-hover standings-table",
          style = "background-color: white; color: black;",
          tags$thead(
            tags$tr(
              tags$th("", style = "width: 60px;"),  # Logo column
              tags$th("Team", style = "text-align: left;"),
              tags$th("W", style = "text-align: center;"),
              tags$th("L", style = "text-align: center;"),
              tags$th("T", style = "text-align: center;"),
              tags$th("PCT", style = "text-align: center;"),
              tags$th("RS", style = "text-align: center;"),
              tags$th("RA", style = "text-align: center;"),
              tags$th("DIFF", style = "text-align: center;")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(data)), function(i) {
              team <- data[i, ]
              run_diff_class <- ifelse(team$RunDifferential > 0, "run-diff-positive",
                                       ifelse(team$RunDifferential < 0, "run-diff-negative", "run-diff-zero"))
              
              tags$tr(
                tags$td(
                  style = "text-align: center;",
                  # Use team-specific logos from www folder
                  tags$img(
                    src = paste0(team$LogoFilename, ".png"), 
                    height = "40px", 
                    alt = paste(team$TeamName, "logo")
                  )
                ),
                tags$td(team$TeamName, style = "text-align: left; font-weight: bold;"),
                tags$td(team$Wins, style = "text-align: center;"),
                tags$td(team$Losses, style = "text-align: center;"),
                tags$td(team$Ties, style = "text-align: center;"),
                tags$td(team$PCT, style = "text-align: center;"),
                tags$td(team$RunsScored, style = "text-align: center;"),
                tags$td(team$RunsAgainst, style = "text-align: center;"),
                tags$td(
                  class = run_diff_class,
                  style = "text-align: center; font-weight: bold;",
                  ifelse(team$RunDifferential > 0, paste0("+", team$RunDifferential), team$RunDifferential)
                )
              )
            })
          )
        )
      )
    })
  })
}