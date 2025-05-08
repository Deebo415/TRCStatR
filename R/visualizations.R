# Emergency Visualizations Module for TRCStatR
# This is a simplified version that avoids complex reactive patterns

# Import required packages
library(shiny)       # For UI components and reactivity
library(shinydashboard) # For dashboard UI elements
library(DT)          # For data tables
library(ggplot2)     # For plotting

# Create internal sample data - using static data to avoid reactive issues
create_sample_data <- function() {
  # Sample player data
  sample_player_data <- data.frame(
    PlayerID = 1:10,
    PlayerName = c("John Smith", "Mike Johnson", "David Williams", "Carlos Rodriguez", 
                  "James Wilson", "Robert Brown", "Michael Davis", "Jose Martinez", 
                  "Thomas Anderson", "Christopher Taylor"),
    TeamID = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
    Position = c("P", "C", "1B", "2B", "SS", "3B", "OF", "OF", "OF", "DH"),
    BattingAvg = c(0.325, 0.287, 0.301, 0.275, 0.315, 0.265, 0.290, 0.310, 0.278, 0.305),
    HomeRuns = c(2, 5, 8, 3, 1, 6, 7, 4, 9, 10),
    RBI = c(15, 22, 35, 18, 12, 28, 30, 20, 38, 42),
    OPS = c(0.850, 0.780, 0.920, 0.750, 0.830, 0.760, 0.800, 0.880, 0.770, 0.900),
    stringsAsFactors = FALSE
  )
  
  # Sample team data
  sample_team_data <- data.frame(
    TeamID = 1:8,
    TeamName = c("Cubs", "Giants", "Guardians", "Mets", "Orioles", "Red Sox", "Royals", "Tigers"),
    Wins = c(12, 10, 8, 9, 11, 7, 6, 5),
    Losses = c(3, 5, 7, 6, 4, 8, 9, 10),
    RunsScored = c(85, 72, 65, 70, 80, 60, 55, 50),
    RunsAllowed = c(45, 55, 70, 65, 50, 75, 80, 85),
    stringsAsFactors = FALSE
  )
  
  # Sample hit data
  sample_hit_data <- data.frame(
    HitID = 1:20,
    PlayerID = sample(1:10, 20, replace = TRUE),
    GameID = sample(1:5, 20, replace = TRUE),
    InningID = sample(1:7, 20, replace = TRUE),
    PitchCount = sample(1:5, 20, replace = TRUE),
    PitchType = sample(c("Fastball", "Curveball", "Slider", "Changeup"), 20, replace = TRUE),
    ExitVelocity = round(runif(20, 60, 105), 1),
    LaunchAngle = round(runif(20, -10, 45), 1),
    Distance = round(runif(20, 0, 350), 0),
    HitResult = sample(c("Single", "Double", "Triple", "Home Run", "Out"), 20, replace = TRUE),
    FieldPosition = sample(1:9, 20, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  return(list(
    player_data = sample_player_data,
    team_data = sample_team_data,
    hit_data = sample_hit_data
  ))
}

# Create a visualization UI function
visualization_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Player Statistics",
        width = 12,
        selectInput(ns("player_select"), "Select Player:", choices = c("Select a player" = "")),
        plotOutput(ns("player_plot"))
      )
    ),
    fluidRow(
      box(
        title = "Team Statistics",
        width = 12,
        selectInput(ns("team_select"), "Select Team:", choices = c("Select a team" = "")),
        plotOutput(ns("team_plot"))
      )
    )
  )
}

# Create a visualization server function
visualization_server <- function(id, player_data = NULL, team_data = NULL, hit_data = NULL, theme = NULL) {
  moduleServer(id, function(input, output, session) {
    # Create static sample data - no reactivity needed
    sample_data <- create_sample_data()
    
    # Static player and team choices to avoid reactive issues
    player_choices <- c("Select a player" = "", 
                       "John Smith" = "1", 
                       "Mike Johnson" = "2", 
                       "David Williams" = "3")
    
    team_choices <- c("Select a team" = "", 
                     "Cubs" = "1", 
                     "Giants" = "2", 
                     "Guardians" = "3")
    
    # Initialize dropdowns with static choices - but only once when the module loads
    # This avoids the reactive context that was causing errors
    isolate({
      # Safely update the player dropdown with static choices
      tryCatch({
        if (!is.null(session) && !is.null(input) && exists("player_select", where = input)) {
          updateSelectInput(session, "player_select", choices = player_choices)
        }
      }, error = function(e) {
        cat("Error updating player select:", e$message, "\n")
      })
      
      # Safely update the team dropdown with static choices
      tryCatch({
        if (!is.null(session) && !is.null(input) && exists("team_select", where = input)) {
          updateSelectInput(session, "team_select", choices = team_choices)
        }
      }, error = function(e) {
        cat("Error updating team select:", e$message, "\n")
      })
    })
    
    # Player plot - simplified to avoid reactive data issues
    output$player_plot <- renderPlot({
      # Only render if a player is selected
      if (input$player_select != "") {
        # Get player ID from selection
        player_id <- as.numeric(input$player_select)
        
        # Create a simple bar plot with static data based on player ID
        # Use different values based on player ID to show some variation
        if (player_id == 1) {
          stats <- c(0.325 * 1000, 5 * 10, 25)
        } else if (player_id == 2) {
          stats <- c(0.290 * 1000, 8 * 10, 35)
        } else {
          stats <- c(0.275 * 1000, 3 * 10, 18)
        }
        
        barplot(
          stats,
          names.arg = c("Batting Avg (x1000)", "HR (x10)", "RBI"),
          col = c("#1f77b4", "#ff7f0e", "#2ca02c"),
          main = paste("Stats for Player", player_id),
          ylim = c(0, 100)
        )
      } else {
        # Empty plot when no player selected
        plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", 
             main = "Select a player to view stats")
      }
    })
    
    # Team plot - simplified to avoid reactive data issues
    output$team_plot <- renderPlot({
      # Only render if a team is selected
      if (input$team_select != "") {
        # Get team ID from selection
        team_id <- as.numeric(input$team_select)
        
        # Create a simple bar plot with static data based on team ID
        # Use different values based on team ID to show some variation
        if (team_id == 1) {
          stats <- c(10, 5, 8, 4)
        } else if (team_id == 2) {
          stats <- c(8, 7, 6, 5)
        } else {
          stats <- c(12, 3, 9, 6)
        }
        
        barplot(
          stats,
          names.arg = c("Wins", "Losses", "Runs Scored (/10)", "Runs Allowed (/10)"),
          col = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
          main = paste("Stats for Team", team_id),
          ylim = c(0, 20)
        )
      } else {
        # Empty plot when no team selected
        plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", 
             main = "Select a team to view stats")
      }
    })
  })
}
