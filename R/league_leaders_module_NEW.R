# League Leaders Module for TRCStatR
# Displays batting, pitching, and WAR leaderboards

library(shiny)
library(dplyr)
library(shinyjs)

# Create a help icon with tooltip for a stat
create_stat_help_icon <- function(stat_name) {
  # Get explanation from the global environment
  stat_explanations <- if (exists("stat_explanations", envir = .GlobalEnv)) {
    get("stat_explanations", envir = .GlobalEnv)
  } else {
    list()
  }
  
  # Get explanation for this stat
  explanation <- stat_explanations[[stat_name]]
  
  # Create a span with the lightbulb icon and tooltip
  span(
    class = "stat-tooltip stat-help-icon",
    title = explanation,
    icon("lightbulb")
  )
}

# UI Module for League Leaders
league_leaders_ui <- function(id) {
  
  # Add minimal CSS just for icon styling
  tooltip_css <- tags$style(HTML("
    /* Lightbulb icon styling */
    .stat-tooltip {
      cursor: help;
      margin-left: 5px;
      color: #f0ad4e; /* Make lightbulb yellow/gold */
    }
    
    /* Custom font for headers */
    .freshman-font {
      font-family: 'Freshman', sans-serif;
    }
  "))
  
  ns <- NS(id)
  
  # Create UI elements
  tagList(
    # Include shinyjs, custom tooltip CSS, and Freshman font
    shinyjs::useShinyjs(),
    tooltip_css,
    
    # Container for the module
    div(
      # Top controls
      fluidRow(
        column(6,
               div(class = "well", style = "padding: 10px; margin-bottom: 15px;",
                   div(style = "display: flex; justify-content: space-between; align-items: center;",
                       div(
                         h3("League Leaders", class = "freshman-font", style = "color: #800000; margin: 0;")
                       ),
                       div(
                         actionButton(ns("refreshLeaderboardsTop"), "Generate or Refresh Leaderboards!", 
                                      icon = icon("sync"), class = "btn-primary"),
                         actionButton(ns("clearDataTop"), "Clear All Data", 
                                      icon = icon("trash"), class = "btn-danger")
                       )
                   )
               )
        )
      ),
      
      # Tabbed interface for different stat categories
      tabsetPanel(
        # Batting Leaders Tab
        tabPanel(
          title = "Batting Leaders",
          fluidRow(
            column(12,
                   h3("Batting Leaders", class = "freshman-font", style = "color: #800000; text-align: center;")
            )
          ),
          
          # First Row - AVG, HR, RBI (3-wide each with 1-wide space between and 1 on each margin)
          fluidRow(
            column(width = 1), # Left margin
            
            # AVG
            column(width = 3,
                   h4(div(
                     "Batting Average (AVG)", 
                     create_stat_help_icon("AVG")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("avg_leaders"))
            ),
            
            column(width = 1), # Space between
            
            # HR
            column(width = 3,
                   h4(div(
                     "Home Runs (HR)", 
                     create_stat_help_icon("HR")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("hr_leaders"))
            ),
            
            column(width = 1), # Space between
            
            # RBI
            column(width = 3,
                   h4(div(
                     "Runs Batted In (RBI)", 
                     create_stat_help_icon("RBI")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("rbi_leaders"))
            ),
            
            column(width = 1) # Right margin
          ),
          
          # Second Row - SB and OBP (4-wide each, 1.5-wide on each margin, 1-wide space between)
          fluidRow(
            column(width = 2), # Left margin (using 2 instead of 1.5)
            
            # SB
            column(width = 4,
                   h4(div(
                     "Stolen Bases (SB)", 
                     create_stat_help_icon("SB")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("sb_leaders"))
            ),
            
            column(width = 1), # Space between
            
            # OBP
            column(width = 4,
                   h4(div(
                     "On-Base Percentage (OBP)", 
                     create_stat_help_icon("OBP")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("obp_leaders"))
            ),
            
            column(width = 1) # Right margin (using 1 instead of 1.5)
          ),
          
          # Third Row - SLG, OPS, OPS+ (3-wide each with 1-wide space between and 1 on each margin)
          fluidRow(
            column(width = 1), # Left margin
            
            # SLG
            column(width = 3,
                   h4(div(
                     "Slugging Percentage (SLG)", 
                     create_stat_help_icon("SLG")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("slg_leaders"))
            ),
            
            column(width = 1), # Space between
            
            # OPS
            column(width = 3,
                   h4(div(
                     "On-Base Plus Slugging (OPS)", 
                     create_stat_help_icon("OPS")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("ops_leaders"))
            ),
            
            column(width = 1), # Space between
            
            # OPS+
            column(width = 3,
                   h4(div(
                     "Normalized OPS (OPS+)", 
                     create_stat_help_icon("OPS+")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("ops_plus_leaders"))
            ),
            
            column(width = 1) # Right margin
          ),
          
          # Fourth Row - wRC+ and wBC (4-wide each, 1.5-wide on each margin, 1-wide space between)
          fluidRow(
            column(width = 2), # Left margin (using 2 instead of 1.5)
            
            # wRC+
            column(width = 4,
                   h4(div(
                     "Weighted Runs Created Plus (wRC+)", 
                     create_stat_help_icon("wRC+")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("wrc_plus_leaders"))
            ),
            
            column(width = 1), # Space between
            
            # wBC
            column(width = 4,
                   h4(div(
                     "Weighted Based Created (wBC)", 
                     create_stat_help_icon("wBC")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("wbc_leaders"))
            ),
            
            column(width = 1) # Right margin (using 1 instead of 1.5)
          )
        ),
        # Pitching Leaders Tab
        tabPanel(
          title = "Pitching Leaders",
          fluidRow(
            column(12,
                   h3("Pitching Leaders", class = "freshman-font", style = "color: #800000; text-align: center;")
            )
          ),
          
          # First Row - ERA, WHIP, Total K (3-wide each with 1-wide space between and 1 on each margin)
          fluidRow(
            column(width = 1), # Left margin
            
            # ERA
            column(width = 3,
                   h4(div(
                     "Earned Run Average (ERA)", 
                     create_stat_help_icon("ERA")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("era_leaders"))
            ),
            
            column(width = 1), # Space between
            
            # WHIP
            column(width = 3,
                   h4(div(
                     "Walks and Hits Per Inning Pitched (WHIP)", 
                     create_stat_help_icon("WHIP")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("whip_leaders"))
            ),
            
            column(width = 1), # Space between
            
            # Total K
            column(width = 3,
                   h4(div(
                     "Total Strikeouts (K)", 
                     create_stat_help_icon("K")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("k_leaders"))
            ),
            
            column(width = 1) # Right margin
          ),
          
          # Second Row - ERA+, WHIP+, K Rate (3-wide each with 1-wide space between and 1 on each margin)
          fluidRow(
            # ERA+
            column(width = 4,
                   h4(div(
                     "Normalized ERA (ERA+)", 
                     create_stat_help_icon("ERA+")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("era_plus_leaders"))
            ),
            
            # WHIP+
            column(width = 4,
                   h4(div(
                     "Normalized WHIP (WHIP+)", 
                     create_stat_help_icon("WHIP+")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("whip_plus_leaders"))
            ),
            
            # K Rate
            column(width = 4,
                   h4(div(
                     "K %", 
                     create_stat_help_icon("K_Pct")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("k_pct_leaders"))
            )
          ),
          
          # Third Row - FIP (4-wide, centered)
          fluidRow(
            column(width = 4), # Left margin
            
            # FIP
            column(width = 4,
                   h4(div(
                     "Fielding Independent Pitching (FIP)", 
                     create_stat_help_icon("FIP")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("fip_leaders"))
            ),
            
            column(width = 4) # Right margin
          )
        ),
        
        # WAR Leaders Tab
        tabPanel(
          title = "WAR Leaders",
          fluidRow(
            column(12,
                   h3("WAR Leaders", class = "freshman-font", style = "color: #800000; text-align: center;"),
                   p("Wins Above Replacement - scaled up to a 162 game season!", 
                     style = "font-style: italic; text-align: center; font-size: 0.9em; color: #666;")
            )
          ),
          
          # First Row - sbWAR and spWAR (4-wide each, 1.5-wide on each margin, 1-wide space between)
          fluidRow(
            column(width = 1), # Left margin
            
            # sbWAR
            column(width = 4,
                   h4(div(
                     "Batting WAR (sbWAR)", 
                     create_stat_help_icon("sbWAR")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("sbwar_leaders"))
            ),
            
            column(width = 2), # Space between
            
            # spWAR
            column(width = 4,
                   h4(div(
                     "Pitching WAR (spWAR)", 
                     create_stat_help_icon("spWAR")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("spwar_leaders"))
            ),
            
            column(width = 1) # Right margin (using 1 instead of 1.5)
          ),
          
          # Second Row - gWAR and oWAR (4-wide each, 1.5-wide on each margin, 1-wide space between)
          fluidRow(
            column(width = 1), # Left margin
            
            # gWAR
            column(width = 4,
                   h4(div(
                     "Contribution to Wins by Bases Created (Gus M. WAR)(gWAR)", 
                     create_stat_help_icon("gWAR")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("gwar_leaders"))
            ),
            
            column(width = 2), # Space between (using 2 instead of 1.5)
            
            # oWAR
            column(width = 4,
                   h4(div(
                     "Offensive WAR (oWAR)", 
                      create_stat_help_icon("oWAR"),
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("owar_leaders"))
            ),
            
            column(width = 1) # Right margin
          ),
          
          # Third Row - wTWAR (4-wide, centered)
          fluidRow(
            column(width = 2), # Left margin
            
            # Total WAR
            column(width = 4,
                   h4(div(
                     "Weighted Total WAR (wTWAR)", 
                     create_stat_help_icon("wTWAR"),
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("war_leaders"))
            ),
            
            column(width = 4,
                   h4(div(
                     "Amazing Heart-Stopping Walk-Off Snags in Center Field", 
                     create_stat_help_icon("amazing_catch")
                   ), class = "freshman-font", style = "color: #800000;"),
                   tableOutput(ns("amazing_catch_leaders"))
            ),
            column(width = 2) # Right margin
          )
        )
      ),
      
      # Bottom controls
      fluidRow(
        column(12,
               div(class = "well", style = "padding: 10px; margin-top: 15px;",
                   div(style = "display: flex; justify-content: space-between; align-items: center;",
                       div(
                         h4("League Leaders", class = "freshman-font", style = "color: #800000; margin: 0;")
                       ),
                       div(
                         actionButton(ns("refreshLeaderboardsBottom"), "Refresh Leaderboards", 
                                      icon = icon("sync"), class = "btn-primary"),
                         actionButton(ns("clearDataBottom"), "Clear All Data", 
                                      icon = icon("trash"), class = "btn-danger")
                       )
                   )
               )
        )
      ),
      
      # Initialize tooltip container and JavaScript for tooltip functionality
      tags$div(id = "tooltip-container", class = "trc-custom-tooltip")
    )
  )
}
# Server Module for League Leaders
league_leaders_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create a reactive value to track when the tab is selected
    tab_selected <- reactiveVal(FALSE)
    
    # Set up an observer that detects when this tab becomes active
    observe({
      # Check if we're in a tabsetPanel context
      if (!is.null(session$userData$tabsetPanel)) {
        current_tab <- session$userData$tabsetPanel()
        # If the current tab is 'League Leaders', set our reactive to TRUE
        if (grepl("league_leaders", current_tab, ignore.case = TRUE)) {
          tab_selected(TRUE)
        }
      }
    })
    
    # When the tab is selected, automatically refresh the leaderboards
    observeEvent(tab_selected(), {
      if (tab_selected()) {
        # Reset the value for next time
        tab_selected(FALSE)
        # Trigger the refresh button
        session$sendInputMessage("refreshLeaderboardsTop", list(value = TRUE))
      }
    })
    
    # Create the amazing catch table
    output$amazing_catch_leaders <- renderTable({
      data.frame(
        Rank = 1,
        Player = "Wesley   \"Willie Mays\"   K",
        Team = "Mets",
        Value = 1,
        stringsAsFactors = FALSE
      )
    }, rownames = FALSE, align = 'l', width = "100%", digits = 0)
    
    # Initialize tooltips with jQuery
    observe({
      # Initialize tooltips when the module loads
      shinyjs::runjs('
        $(document).ready(function() {
          /* Create tooltip element if it doesn\'t exist */
          if ($("#tooltip-container").length === 0) {
            $("body").append("<div id=\'tooltip-container\' class=\'trc-custom-tooltip\'></div>");
          }
          
          /* Set up event handlers for tooltip icons */
          $(document).on("mouseenter", ".stat-help-icon", function(e) {
            var tooltipText = $(this).attr("data-tooltip");
            if (tooltipText) {
              $("#tooltip-container").text(tooltipText).show();
              updateTooltipPosition(e);
            }
          });
          
          $(document).on("mouseleave", ".stat-help-icon", function() {
            $("#tooltip-container").hide();
          });
          
          $(document).on("mousemove", function(e) {
            if ($("#tooltip-container").is(":visible")) {
              updateTooltipPosition(e);
            }
          });
          
          function updateTooltipPosition(e) {
            var tooltip = $("#tooltip-container");
            
            /* Position tooltip closer to cursor */
            var x = e.pageX + 5;
            var y = e.pageY - tooltip.outerHeight() - 5;
            
            /* If tooltip would appear above the viewport, show it below the cursor instead */
            if (y < 0) {
              y = e.pageY + 5;
            }
            
            /* Ensure tooltip stays within viewport horizontally */
            var tooltipWidth = tooltip.outerWidth();
            var windowWidth = $(window).width();
            
            if (x + tooltipWidth > windowWidth) {
              x = windowWidth - tooltipWidth - 5;
            }
            
            /* Make sure tooltip doesn\'t go off-screen to the left */
            if (x < 0) {
              x = 5;
            }
            
            /* Apply the position with a smooth transition */
            tooltip.css({
              left: x + "px",
              top: y + "px",
              transition: "left 0.1s, top 0.1s"
            });
          }
        });
      ')
    })
    
    # Simplified function to create a leaderboard table with player hyperlinks
    createLeaderboardTable <- function(data, column_name, desc = TRUE, format_func = NULL, top_n = 10) {
      # Create a safe empty table as default
      empty_table <- data.frame(
        Rank = integer(0),
        Player = character(0),
        Team = character(0),
        Value = character(0),
        stringsAsFactors = FALSE
      )
      
      # Check for valid input
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0 || !column_name %in% names(data)) {
        return(empty_table)
      }
      
      # Check if Team column exists in the data
      has_team_column <- "Team" %in% names(data)
      
      # Extract only the columns we need
      result_data <- data.frame(
        PlayerID = data$PlayerID,
        Player = data$Player,
        Value = data[[column_name]],
        stringsAsFactors = FALSE
      )
      
      # Add Team column if it exists in the data
      if (has_team_column) {
        result_data$Team <- data$Team
      } else {
        result_data$Team <- "Unknown"
      }
      
      # Remove rows with NA values in the target column
      result_data <- result_data[!is.na(result_data$Value), ]
      
      if (nrow(result_data) == 0) {
        return(empty_table)
      }
      
      # Special handling for ERA+ and WHIP+ when ERA or WHIP is 0
      if (column_name == "ERA_plus" && "ERA" %in% names(data)) {
        # For each row in result_data, check if the corresponding ERA in data is 0
        for (i in seq_len(nrow(result_data))) {
          player_id <- result_data$PlayerID[i]
          player_row <- which(data$PlayerID == player_id)
          if (length(player_row) > 0 && !is.na(data$ERA[player_row[1]]) && data$ERA[player_row[1]] == 0) {
            result_data$Value[i] <- Inf  # Set to Infinity for sorting purposes
          }
        }
      }
      
      if (column_name == "WHIP_plus" && "WHIP" %in% names(data)) {
        # For each row in result_data, check if the corresponding WHIP in data is 0
        for (i in seq_len(nrow(result_data))) {
          player_id <- result_data$PlayerID[i]
          player_row <- which(data$PlayerID == player_id)
          if (length(player_row) > 0 && !is.na(data$WHIP[player_row[1]]) && data$WHIP[player_row[1]] == 0) {
            result_data$Value[i] <- Inf  # Set to Infinity for sorting purposes
          }
        }
      }
      
      # Sort data by the stat column
      if (desc) {
        result_data <- result_data[order(result_data$Value, decreasing = TRUE), ]
      } else {
        result_data <- result_data[order(result_data$Value), ]
      }
      
      # Take top N rows
      if (nrow(result_data) > top_n) {
        result_data <- result_data[1:top_n, ]
      }
      
      # Add rank
      result_data$Rank <- seq_len(nrow(result_data))
      
      # Format the value if a format function is provided
      if (!is.null(format_func)) {
        result_data$Value <- as.character(sapply(result_data$Value, function(x) {
          if (is.infinite(x)) return("Inf")
          format_func(x)
        }))
      } else {
        result_data$Value <- as.character(sapply(result_data$Value, function(x) {
          if (is.infinite(x)) return("Inf")
          x
        }))
      }
      
      # Use just the player name and team without ID
      result_data$Player <- as.character(result_data$Player)
      result_data$Team <- as.character(result_data$Team)
      
      # Select only the columns we want to display
      final_data <- data.frame(
        Rank = result_data$Rank,
        Player = result_data$Player,
        Team = result_data$Team,
        Value = result_data$Value,
        stringsAsFactors = FALSE
      )
      
      return(final_data)
    }
    
    # We don't need this function anymore since we're using direct renderTable calls
    # Function left as a placeholder in case we need to revert
    
    # Function to clear all tables
    clearAllTables <- function() {
      # Create a standard empty table with Team column
      empty_table <- data.frame(
        Rank = integer(0), 
        Player = character(0), 
        Team = character(0),
        Value = character(0),
        stringsAsFactors = FALSE
      )
      
      # Clear all batting tables
      output$avg_leaders <- renderTable(empty_table, rownames = FALSE)
      output$obp_leaders <- renderTable(empty_table, rownames = FALSE)
      output$slg_leaders <- renderTable(empty_table, rownames = FALSE)
      output$ops_leaders <- renderTable(empty_table, rownames = FALSE)
      output$hr_leaders <- renderTable(empty_table, rownames = FALSE)
      output$rbi_leaders <- renderTable(empty_table, rownames = FALSE)
      output$sb_leaders <- renderTable(empty_table, rownames = FALSE)
      output$ops_plus_leaders <- renderTable(empty_table, rownames = FALSE)
      output$wrc_plus_leaders <- renderTable(empty_table, rownames = FALSE)
      output$wbc_leaders <- renderTable(empty_table, rownames = FALSE)
      
      # Clear all pitching tables
      output$era_leaders <- renderTable(empty_table, rownames = FALSE)
      output$whip_leaders <- renderTable(empty_table, rownames = FALSE)
      output$k_leaders <- renderTable(empty_table, rownames = FALSE)
      output$k_pct_leaders <- renderTable(empty_table, rownames = FALSE)
      output$era_plus_leaders <- renderTable(empty_table, rownames = FALSE)
      output$whip_plus_leaders <- renderTable(empty_table, rownames = FALSE)
      output$fip_leaders <- renderTable(empty_table, rownames = FALSE)
      
      # Clear all WAR tables
      output$sbwar_leaders <- renderTable(empty_table, rownames = FALSE)
      output$spwar_leaders <- renderTable(empty_table, rownames = FALSE)
      output$gwar_leaders <- renderTable(empty_table, rownames = FALSE)
      output$owar_leaders <- renderTable(empty_table, rownames = FALSE)
      output$war_leaders <- renderTable(empty_table, rownames = FALSE)
    }
    
    # Initialize with empty tables
    observe({
      clearAllTables()
    })
    
    # Handle the refresh leaderboards button
    observeEvent(input$refreshLeaderboardsTop, {
      # Show loading message
      showModal(modalDialog(
        title = "Loading Leaderboards",
        "Please wait while we load the latest stats...",
        footer = NULL,
        easyClose = FALSE
      ))
      
      # For debugging
      print("Refreshing leaderboards...")
      
      tryCatch({
        # Format functions for different stat types
        format_avg <- function(x) sprintf("%.3f", x)
        format_era <- function(x) sprintf("%.2f", x)
        format_plus <- function(x) sprintf("%d", round(x))
        format_war <- function(x) sprintf("%.1f", x)
        format_pct <- function(x) sprintf("%.1f%%", x * 100)
        
        # Get master tables from global environment
        if (exists("master_batting_stats", envir = .GlobalEnv) && 
            exists("master_pitching_stats", envir = .GlobalEnv) && 
            exists("master_war_stats", envir = .GlobalEnv)) {
          
          # Get all master tables
          batting_stats <- get("master_batting_stats", envir = .GlobalEnv)
          pitching_stats <- get("master_pitching_stats", envir = .GlobalEnv)
          war_stats <- get("master_war_stats", envir = .GlobalEnv)
          
          print(paste("Found master tables in global environment:", 
                      nrow(batting_stats), "batting records,", 
                      nrow(pitching_stats), "pitching records,", 
                      nrow(war_stats), "WAR records"))
          
          # Now render all tables if we have data
          if (nrow(batting_stats) > 0) {
            print("Rendering batting tables...")
            
            # Render all batting tables
            output$avg_leaders <- renderTable(createLeaderboardTable(batting_stats, "AVG", TRUE, format_avg), rownames = FALSE)
            output$hr_leaders <- renderTable(createLeaderboardTable(batting_stats, "HR", TRUE), rownames = FALSE)
            output$rbi_leaders <- renderTable(createLeaderboardTable(batting_stats, "RBI", TRUE), rownames = FALSE)
            output$sb_leaders <- renderTable(createLeaderboardTable(batting_stats, "SB", TRUE), rownames = FALSE)
            output$obp_leaders <- renderTable(createLeaderboardTable(batting_stats, "OBP", TRUE, format_avg), rownames = FALSE)
            output$slg_leaders <- renderTable(createLeaderboardTable(batting_stats, "SLG", TRUE, format_avg), rownames = FALSE)
            output$ops_leaders <- renderTable(createLeaderboardTable(batting_stats, "OPS", TRUE, format_avg), rownames = FALSE)
            output$ops_plus_leaders <- renderTable(createLeaderboardTable(batting_stats, "OPS_plus", TRUE, format_plus), rownames = FALSE)
            output$wrc_plus_leaders <- renderTable(createLeaderboardTable(batting_stats, "wRC_plus", TRUE, format_plus), rownames = FALSE)
            output$wbc_leaders <- renderTable(createLeaderboardTable(batting_stats, "wBC", TRUE, function(x) sprintf("%.1f", x)), rownames = FALSE)
          }
          
          # Pitching Leaders
          if (nrow(pitching_stats) > 0) {
            print("Rendering pitching tables...")
            output$era_leaders <- renderTable(createLeaderboardTable(pitching_stats, "ERA", FALSE, format_era), rownames = FALSE)
            output$whip_leaders <- renderTable(createLeaderboardTable(pitching_stats, "WHIP", FALSE, format_avg), rownames = FALSE)
            output$k_leaders <- renderTable(createLeaderboardTable(pitching_stats, "Pitch_K", TRUE), rownames = FALSE)
            output$k_pct_leaders <- renderTable(createLeaderboardTable(pitching_stats, "K_pct", TRUE, format_pct), rownames = FALSE)
            output$era_plus_leaders <- renderTable(createLeaderboardTable(pitching_stats, "ERA_plus", TRUE, format_plus), rownames = FALSE)
            output$whip_plus_leaders <- renderTable(createLeaderboardTable(pitching_stats, "WHIP_plus", TRUE, format_plus), rownames = FALSE)
            output$fip_leaders <- renderTable(createLeaderboardTable(pitching_stats, "FIP", FALSE, format_era), rownames = FALSE)
          }
          
          # WAR Leaders
          if (nrow(war_stats) > 0) {
            print("Rendering WAR tables...")
            output$sbwar_leaders <- renderTable(createLeaderboardTable(war_stats, "sbWAR", TRUE, format_war), rownames = FALSE)
            output$spwar_leaders <- renderTable(createLeaderboardTable(war_stats, "spWAR", TRUE, format_war), rownames = FALSE)
            output$gwar_leaders <- renderTable(createLeaderboardTable(war_stats, "gWAR", TRUE, format_war), rownames = FALSE)
            output$owar_leaders <- renderTable(createLeaderboardTable(war_stats, "oWAR", TRUE, format_war), rownames = FALSE)
            output$war_leaders <- renderTable(createLeaderboardTable(war_stats, "wTWAR", TRUE, format_war), rownames = FALSE)
          }
        } else {
          # Fallback if master tables don't exist
          message("Warning: One or more master tables not found in global environment.")
          removeModal()
          return(NULL)
        }
      }, error = function(e) {
        message("Error loading leaderboards: ", e$message)
        removeModal()
      })
      
      # Remove loading message
      removeModal()
    })
    
    # Handle the bottom refresh button (same action as top button)
    observeEvent(input$refreshLeaderboardsBottom, {
      # Trigger the same action as the top button
      session$sendInputMessage("refreshLeaderboardsTop", list(value = TRUE))
    })
    
    # Handle the top clear data button
    observeEvent(input$clearDataTop, {
      # Show confirmation dialog
      showModal(modalDialog(
        title = "Confirm Clear Data",
        "Are you sure you want to clear all leaderboard data?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirmClear"), "Yes, Clear All Data", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })
    
    # Handle the bottom clear data button (same confirmation dialog)
    observeEvent(input$clearDataBottom, {
      # Trigger the same action as the top button
      session$sendInputMessage("clearDataTop", list(value = TRUE))
    })
    
    # Handle the confirmation to clear data
    observeEvent(input$confirmClear, {
      # Clear all tables
      clearAllTables()
      
      # Close the confirmation dialog
      removeModal()
    })
  })
}