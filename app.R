# TRCStatR - Main Application File
# A focused Shiny application for youth baseball statistics (ages 11-13)
# Uses RDS files for data storage

# Global configuration for deployment
options(warn = -1)  # Suppress warnings completely for deployment

# Global error handler to prevent app crashes
options(shiny.sanitize.errors = TRUE)  # Sanitize error messages for production
options(shiny.error = function() {
  # Custom error handler that logs errors but doesn't crash the app
  msg <- paste("Error in TRCStatR:", geterrmessage())
  cat(msg, file = stderr())
  # Return a safe value that won't crash the app
  NULL
})

# Load only essential packages for deployment
tryCatch({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(DT)
  library(dplyr)
  
  # Load additional packages only if the essential ones loaded successfully
  packages_to_try <- c(
    "shinyWidgets", "shinycssloaders", "shinyalert", "shinybusy", 
    "shinyvalidate", "shinytoastr", "shinyFeedback", "shinythemes", 
    "shinyFiles", "shinyAce", "tidyr", "later"
  )
  
  for (pkg in packages_to_try) {
    tryCatch({
      library(pkg, character.only = TRUE)
    }, error = function(e) {
      # Skip packages that fail to load
    })
  }
}, error = function(e) {
  # If we can't load essential packages, we'll still try to show a minimal UI
  cat("Error loading essential packages:", e$message, "\n", file = stderr())
})

# Load source files with error handling
load_source <- function(file_path) {
  tryCatch({
    source(file_path)
    TRUE
  }, error = function(e) {
    cat("Error loading", file_path, ":", e$message, "\n", file = stderr())
    FALSE
  })
}

# Try to load each module, but continue even if some fail
modules_loaded <- c(
  object_engine = load_source("R/object_engine.R"),
  stats = load_source("R/stats.R"),
  advanced_metrics = load_source("R/advanced_metrics_module.R"),
  league_standings = load_source("R/league_standings.R"),
  power_rankings = load_source("R/power_rankings.R"),
  league_leaders = load_source("R/league_leaders_module_NEW.R"),
  team_snapshot = load_source("R/team_snapshot_simple.R"),
  player_card = load_source("R/player_card_module.R")
)

# Initialize empty data frames with proper structure
tryCatch({
  # Create empty data frames for all required tables with minimal structure
  assign("games", data.frame(GameID = integer(0), GameDate = character(0), stringsAsFactors = FALSE), envir = .GlobalEnv)
  assign("teams", data.frame(TeamID = integer(0), TeamName = character(0), stringsAsFactors = FALSE), envir = .GlobalEnv)
  assign("players", data.frame(PlayerID = integer(0), FirstName = character(0), LastInitial = character(0), TeamID = integer(0), Team = character(0), stringsAsFactors = FALSE), envir = .GlobalEnv)
  assign("batting", data.frame(PlayerID = integer(0), AB = integer(0), H = integer(0), stringsAsFactors = FALSE), envir = .GlobalEnv)
  assign("pitching", data.frame(PlayerID = integer(0), OutsRecorded = integer(0), stringsAsFactors = FALSE), envir = .GlobalEnv)
  
  # Try to load data files with error handling
  load_data_file <- function(file_path, var_name) {
    if (file.exists(file_path)) {
      tryCatch({
        data <- readRDS(file_path)
        assign(var_name, data, envir = .GlobalEnv)
        TRUE
      }, error = function(e) {
        cat("Error loading", file_path, ":", e$message, "\n", file = stderr())
        FALSE
      })
    } else {
      FALSE
    }
  }
  
  # Try to load each data file
  data_loaded <- c(
    games = load_data_file("tblGames.rds", "games"),
    teams = load_data_file("tblTeams.rds", "teams"),
    players = load_data_file("tblPlayers.rds", "players"),
    batting = load_data_file("tblBattingStatsFlat.rds", "batting"),
    pitching = load_data_file("tblPitchingStatsFlat.rds", "pitching")
  )
}, error = function(e) {
  # Log the error but continue
  cat("Error initializing data frames:", e$message, "\n", file = stderr())
})

# Create accessor functions for data instead of global extractions

# Define data accessor functions
get_games <- function() {
  if (exists("games", envir = .GlobalEnv)) {
    return(get("games", envir = .GlobalEnv))
  }
  return(NULL)
}

get_teams <- function() {
  if (exists("teams", envir = .GlobalEnv)) {
    return(get("teams", envir = .GlobalEnv))
  }
  return(NULL)
}

get_players <- function() {
  if (exists("players", envir = .GlobalEnv)) {
    return(get("players", envir = .GlobalEnv))
  }
  return(NULL)
}

get_batting_stats <- function() {
  if (exists("batting", envir = .GlobalEnv)) {
    return(get("batting", envir = .GlobalEnv))
  }
  return(NULL)
}

get_pitching_stats <- function() {
  if (exists("pitching", envir = .GlobalEnv)) {
    return(get("pitching", envir = .GlobalEnv))
  }
  return(NULL)
}

get_team_colors <- function() {
  if (exists("team_colors", envir = .GlobalEnv)) {
    return(get("team_colors", envir = .GlobalEnv))
  }
  return(NULL)
}

get_stat_explanations <- function() {
  if (exists("stat_explanations", envir = .GlobalEnv)) {
    return(get("stat_explanations", envir = .GlobalEnv))
  }
  return(list())
}

# Ensure all_ids is available in the global environment
if (!exists("all_ids", envir = .GlobalEnv)) {
  all_ids <- players$PlayerID
  assign("all_ids", all_ids, envir = .GlobalEnv)
}

# Define these global functions that depend on environment variables
home_batting_order_labels <- function() {
  if (exists(".home_batting_order_labels", envir = .GlobalEnv)) {
    get(".home_batting_order_labels", envir = .GlobalEnv)
  } else {
    character(0)  # Return empty vector if not yet initialized
  }
}

away_batting_order_labels <- function() {
  if (exists(".away_batting_order_labels", envir = .GlobalEnv)) {
    get(".away_batting_order_labels", envir = .GlobalEnv)
  } else {
    character(0)  # Return empty vector if not yet initialized
  }
}

home_pitching_order_labels <- function() {
  if (exists(".home_pitching_order_labels", envir = .GlobalEnv)) {
    get(".home_pitching_order_labels", envir = .GlobalEnv)
  } else {
    character(0)  # Return empty vector if not yet initialized
  }
}

away_pitching_order_labels <- function() {
  if (exists(".away_pitching_order_labels", envir = .GlobalEnv)) {
    get(".away_pitching_order_labels", envir = .GlobalEnv)
  } else {
    character(0)  # Return empty vector if not yet initialized
  }
}

# Create a theme for the application
app_theme <- tryCatch({
  create_trc_theme(
    primary_color = "#800000",    # Maroon
    secondary_color = "#AB4E52",  # Lighter Maroon
    accent_color = "#FFFFFF"      # White
  )
}, error = function(e) {
  # Use default theme if creation fails
  list(
    primary = "#800000",
    secondary = "#AB4E52",
    accent = "#FFFFFF"
  )
})

# Define UI components for features with different access levels
restricted_access_ui <- function(feature_name, message_type = "coming_soon") {
  if (message_type == "coming_soon") {
    div(
      style = "text-align: center; padding: 50px 20px;",
      img(src = "construction.png", height = 100, alt = "Under Construction", style = "margin-bottom: 20px;"),
      h3(paste(feature_name, "Coming Soon!"), style = "font-family: 'Freshman', sans-serif; color: #800000;"),
      p("This feature is currently under development and will be available in a future update.", 
        style = "font-size: 16px; max-width: 500px; margin: 0 auto;"),
      p("Check back later for updates!", style = "font-size: 14px; margin-top: 15px;")
    )
  } else if (message_type == "restricted") {
    div(
      style = "text-align: center; padding: 50px 20px;",
      img(src = "locked.png", height = 100, alt = "Restricted Access", style = "margin-bottom: 20px;"),
      h3(paste(feature_name, "- Restricted Access"), style = "font-family: 'Freshman', sans-serif; color: #800000;"),
      p("This feature is restricted to authorized league statisticians only.", 
        style = "font-size: 16px; max-width: 500px; margin: 0 auto;"),
      p("If you need access to this feature, please contact the league administrator.", 
        style = "font-size: 14px; margin-top: 15px;")
    )
  }
}

# Create the base UI
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = div(class = "empty-title"),
    titleWidth = 230,
    tags$li(
      class = "dropdown app-title",
      tags$div(
        tags$img(src = "tiger_logo.png", height = 50, alt = "TRC Tiger Logo", class = "logo-img"),
        tags$span(class = "title-text", "TRCStatR"),
        tags$span(class = "version-badge", "Version 1.1 - Added Power Rankings and League Leaders Auto-load", 
                  style = "font-size: 12px; background: #4b0082; color: white; padding: 2px 6px; border-radius: 10px; margin-left: 8px;")
      ),
      style = "position: absolute; right: 20px; top: 10px;"
    )
  ),
  
  dashboardSidebar(
    width = 230,
    div(
      class = "sidebar-content",
      div(
        class = "sidebar-credits",
        style = "margin-bottom: 18px;",
        HTML("  Statistician: Sean Dobbs<br>  SABRmetrician: Augustine &quot;Gus&quot; &quot;G&quot; Murphy<br><span style='font-size:11px;'>(also a player for the 11/12 Tigers)</span>")
      ),
      sidebarMenu(
        id = "mainMenu",
        menuItem("League Standings", tabName = "standings", icon = icon("table")),
        menuItem("Player Cards", tabName = "playercards", icon = icon("id-card")),
        menuItem("League Leaders", tabName = "leaders", icon = icon("trophy")),
        menuItem("Team Snapshot", tabName = "team_snapshot", icon = icon("chart-bar")),
        # Greyed out menu items
        menuItem(HTML("Advanced Metrics<br>and Baseball Career Lab"), tabName = "baseball_lab", icon = icon("chart-line"), 
                 # Add disabled class and tooltip
                 class = "disabled-menu-item",
                 tags$span("Coming Soon", class = "menu-tooltip")
        ),
        menuItem("View/Edit", tabName = "viewgames", icon = icon("edit"),
                 # Add disabled class and tooltip
                 class = "disabled-menu-item",
                 tags$span("Restricted Access", class = "menu-tooltip")
        )
      ),
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(HTML('.large-numeric-input input { font-size: 1.5em !important; height: 2.5em !important; }')),
      tags$script(src = "https://unpkg.com/react@17/umd/react.production.min.js"),
      tags$script(src = "https://unpkg.com/react-dom@17/umd/react-dom.production.min.js"),
      tags$script(src = "js/PlayerCard.js"),
      tags$script(src = "https://cdn.plot.ly/plotly-latest.min.js")
    ),
    tags$script(HTML("")),
    tags$head(
      tags$script('
    $(document).on("click", "button[id^=\'remove_\']", function(event) {
      Shiny.onInputChange("clicked_remove_button", this.id, {priority: "event"});
    });
  ')
    ),
    
    tabItems(
      tabItem(tabName = "playercards",
              fluidRow(
                box(
                  title = "Player Cards",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("Generate player cards with detailed statistics for any player in the league."),
                  p("Select a team and player, then click 'Generate Player Card' to view their stats."),
                  player_card_ui("standalone_player_card")
                )
              )
      ),
      
      tabItem(tabName = "baseball_lab",
              baseball_lab_ui("baseball_lab")
      ),
      
      tabItem(tabName = "team_snapshot",
              fluidRow(
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  team_snapshot_ui_simple("team_snapshot_simple")
                )
              )
      ),
      
      tabItem(tabName = "viewgames",
              restricted_access_ui("Game Viewer", "coming_soon")
      ),
      
      tabItem(tabName = "standings",
              fluidRow(
                box(
                  title = "League Standings",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  league_standings_ui("league_standings")
                ),
                box(
                  title = "Power Rankings",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  power_rankings_ui("power_rankings")
                )
              )
      ),
      
      tabItem(tabName = "leaders",
              fluidRow(
                box(
                  title = tags$span(class = "freshman-font", "League Leaders"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  league_leaders_ui("league_leaders")
                )
              )
      )
    ) # Close tabItems
  ) # Close dashboardBody
)  

# --- SERVER LOGIC ---

library(pins)

server <- function(input, output, session) {
  # Initialize on startup with comprehensive error handling
  tryCatch({
    observe({
      # Wrap everything in tryCatch to prevent startup failures
      tryCatch({
        # Try to update completed games
        if (exists("update_completed_games")) {
          tryCatch({
            update_completed_games()
          }, error = function(e) {
            # Log but continue if this fails
            cat("Error updating completed games:", e$message, "\n", file = stderr())
          })
        }
        
        # Try to create master tables
        if (exists("create_master_tables", envir = .GlobalEnv)) {
          tryCatch({
            suppressMessages(create_master_tables())
          }, error = function(e) {
            # Log but continue if this fails
            cat("Error creating master tables:", e$message, "\n", file = stderr())
          })
        }
      }, error = function(e) {
        # Catch any other errors in the observer
        cat("Error in startup observer:", e$message, "\n", file = stderr())
      })
    })
  }, error = function(e) {
    # Catch errors in setting up the observer itself
    cat("Failed to set up startup observer:", e$message, "\n", file = stderr())
  })
  
  # --- 1. REACTIVE VALUES ---
  # Initialize reactive values to track player lineups and game state
  home_batters <- reactiveVal(character(0))
  away_batters <- reactiveVal(character(0))
  home_pitchers <- reactiveVal(character(0))
  away_pitchers <- reactiveVal(character(0))
  
  # These track the order of players
  home_batting_order <- reactiveVal(character(0))
  away_batting_order <- reactiveVal(character(0))
  home_pitching_order <- reactiveVal(character(0))
  away_pitching_order <- reactiveVal(character(0))
  
  # Game state tracking
  has_players_added <- reactiveVal(FALSE)
  current_game_details <- reactiveVal(NULL)
  restored_flag <- reactiveVal(FALSE)
  
  # Initialize the global variables for player order labels
  # These are used by the sortable interface
  assign(".home_batting_order_labels", character(0), envir = .GlobalEnv)
  assign(".away_batting_order_labels", character(0), envir = .GlobalEnv)
  assign(".home_pitching_order_labels", character(0), envir = .GlobalEnv)
  assign(".away_pitching_order_labels", character(0), envir = .GlobalEnv)
  
  # --- 2. HELPER FUNCTIONS FOR UI GENERATION ---
  
  # Generate UI for a batter entry - now using global stats
  createBatterUI <- function(side, pid, player_choices, selected = NULL) {
    entry_id <- paste0(side, "_batting_entry_", pid)
    batter_id <- paste0(side, "Batter_", pid)
    
    # Get any existing stats from global vectors - used for initial values
    init_values <- list(
      PA = 0, X1B = 0, X2B = 0, X3B = 0, HR = 0, BB = 0, K = 0, 
      RBI = 0, HBP = 0, SF = 0, SH = 0, GIDP = 0, SB = 0, CS = 0, ROE = 0
    )
    
    if (!is.null(selected)) {
      player_id_char <- as.character(selected)
      
      # Try to get initial values for this player from global vectors
      if (exists("PA", envir = .GlobalEnv)) {
        pa_vec <- get("PA", envir = .GlobalEnv)
        if (player_id_char %in% names(pa_vec)) init_values$PA <- pa_vec[player_id_char]
      }
      
      if (exists("singles", envir = .GlobalEnv)) {
        singles_vec <- get("singles", envir = .GlobalEnv)
        if (player_id_char %in% names(singles_vec)) init_values$X1B <- singles_vec[player_id_char]
      }
      
      if (exists("doubles", envir = .GlobalEnv)) {
        doubles_vec <- get("doubles", envir = .GlobalEnv)
        if (player_id_char %in% names(doubles_vec)) init_values$X2B <- doubles_vec[player_id_char]
      }
      
      if (exists("triples", envir = .GlobalEnv)) {
        triples_vec <- get("triples", envir = .GlobalEnv)
        if (player_id_char %in% names(triples_vec)) init_values$X3B <- triples_vec[player_id_char]
      }
      
      if (exists("home_runs", envir = .GlobalEnv)) {
        hr_vec <- get("home_runs", envir = .GlobalEnv)
        if (player_id_char %in% names(hr_vec)) init_values$HR <- hr_vec[player_id_char]
      }
      
      if (exists("walks", envir = .GlobalEnv)) {
        bb_vec <- get("walks", envir = .GlobalEnv)
        if (player_id_char %in% names(bb_vec)) init_values$BB <- bb_vec[player_id_char]
      }
      
      if (exists("strikeouts", envir = .GlobalEnv)) {
        k_vec <- get("strikeouts", envir = .GlobalEnv)
        if (player_id_char %in% names(k_vec)) init_values$K <- k_vec[player_id_char]
      }
      
      if (exists("rbis", envir = .GlobalEnv)) {
        rbi_vec <- get("rbis", envir = .GlobalEnv)
        if (player_id_char %in% names(rbi_vec)) init_values$RBI <- rbi_vec[player_id_char]
      }
      
      if (exists("hit_by_pitch", envir = .GlobalEnv)) {
        hbp_vec <- get("hit_by_pitch", envir = .GlobalEnv)
        if (player_id_char %in% names(hbp_vec)) init_values$HBP <- hbp_vec[player_id_char]
      }
      
      if (exists("sac_flies", envir = .GlobalEnv)) {
        sf_vec <- get("sac_flies", envir = .GlobalEnv)
        if (player_id_char %in% names(sf_vec)) init_values$SF <- sf_vec[player_id_char]
      }
      
      if (exists("sac_hits", envir = .GlobalEnv)) {
        sh_vec <- get("sac_hits", envir = .GlobalEnv)
        if (player_id_char %in% names(sh_vec)) init_values$SH <- sh_vec[player_id_char]
      }
      
      if (exists("grounded_into_dp", envir = .GlobalEnv)) {
        gidp_vec <- get("grounded_into_dp", envir = .GlobalEnv)
        if (player_id_char %in% names(gidp_vec)) init_values$GIDP <- gidp_vec[player_id_char]
      }
      
      if (exists("steals", envir = .GlobalEnv)) {
        sb_vec <- get("steals", envir = .GlobalEnv)
        if (player_id_char %in% names(sb_vec)) init_values$SB <- sb_vec[player_id_char]
      }
      
      if (exists("caught_stealing", envir = .GlobalEnv)) {
        cs_vec <- get("caught_stealing", envir = .GlobalEnv)
        if (player_id_char %in% names(cs_vec)) init_values$CS <- cs_vec[player_id_char]
      }
      
      if (exists("reached_on_error", envir = .GlobalEnv)) {
        roe_vec <- get("reached_on_error", envir = .GlobalEnv)
        if (player_id_char %in% names(roe_vec)) init_values$ROE <- roe_vec[player_id_char]
      }
    }
    
    tagList(
      div(
        id = entry_id,
        class = "batter-entry player-stats-entry",
        style = "padding: 8px; border: 1px solid #ddd; border-radius: 5px; background: #fff; margin-bottom: 8px;",
        
        # Single row with all stats and player selection
        div(style = "display: flex; align-items: center; flex-wrap: nowrap; overflow-x: auto; gap: 5px; margin-bottom: 8px;",
            selectInput(batter_id, NULL, choices = player_choices, selected = selected, width = "110px"),
            div(class = 'large-numeric-input', numericInput(paste0(batter_id, "_PA"), "PA", init_values$PA, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(batter_id, "_1B"), "1B", init_values$X1B, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(batter_id, "_2B"), "2B", init_values$X2B, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(batter_id, "_3B"), "3B", init_values$X3B, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(batter_id, "_HR"), "HR", init_values$HR, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(batter_id, "_BB"), "BB", init_values$BB, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(batter_id, "_K"), "K", init_values$K, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(batter_id, "_RBI"), "RBI", init_values$RBI, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(batter_id, "_HBP"), "HBP", init_values$HBP, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(batter_id, "_SF"), "SF", init_values$SF, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(batter_id, "_SH"), "SH", init_values$SH, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(batter_id, "_GIDP"), "GIDP", init_values$GIDP, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(batter_id, "_SB"), "SB", init_values$SB, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(batter_id, "_CS"), "CS", init_values$CS, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(batter_id, "_ROE"), "ROE", init_values$ROE, min = 0, width = "60px")),
            actionButton(paste0("remove_", entry_id), "", icon = icon("times"), 
                         class = "btn-danger btn-sm", 
                         style = "margin-left: 10px;")
        ),
        
        # Stats display at bottom - now using global calculators
        div(
          style = "margin-top: 5px; padding: 5px; background: #f8f9fa; border-radius: 3px;",
          fluidRow(
            column(6, textOutput(paste0(batter_id, "_AVG"))),
            column(6, textOutput(paste0(batter_id, "_OBP")))
          )
        )
      )
    )
  }
  
  # Generate UI for a pitcher entry - now using global stats
  createPitcherUI <- function(side, pid, player_choices, selected = NULL) {
    entry_id <- paste0(side, "_pitching_entry_", pid)
    pitcher_id <- paste0(side, "Pitcher_", pid)
    
    # Get any existing stats from global vectors - used for initial values
    init_values <- list(
      OR = 0, BF = 0, H = 0, R = 0, ER = 0, BB = 0, K = 0, 
      HR = 0, HBP =.0, GB = 0, WP = 0
    )
    
    if (!is.null(selected)) {
      player_id_char <- as.character(selected)
      
      # Try to get initial values from global vectors
      if (exists("outs_recorded", envir = .GlobalEnv)) {
        or_vec <- get("outs_recorded", envir = .GlobalEnv)
        if (player_id_char %in% names(or_vec)) init_values$OR <- or_vec[player_id_char]
      }
      
      if (exists("batters_faced", envir = .GlobalEnv)) {
        bf_vec <- get("batters_faced", envir = .GlobalEnv)
        if (player_id_char %in% names(bf_vec)) init_values$BF <- bf_vec[player_id_char]
      }
      
      if (exists("hits_allowed", envir = .GlobalEnv)) {
        h_vec <- get("hits_allowed", envir = .GlobalEnv)
        if (player_id_char %in% names(h_vec)) init_values$H <- h_vec[player_id_char]
      }
      
      if (exists("runs_allowed", envir = .GlobalEnv)) {
        r_vec <- get("runs_allowed", envir = .GlobalEnv)
        if (player_id_char %in% names(r_vec)) init_values$R <- r_vec[player_id_char]
      }
      
      if (exists("earned_runs", envir = .GlobalEnv)) {
        er_vec <- get("earned_runs", envir = .GlobalEnv)
        if (player_id_char %in% names(er_vec)) init_values$ER <- er_vec[player_id_char]
      }
      
      if (exists("walks_allowed", envir = .GlobalEnv)) {
        bb_vec <- get("walks_allowed", envir = .GlobalEnv)
        if (player_id_char %in% names(bb_vec)) init_values$BB <- bb_vec[player_id_char]
      }
      
      if (exists("batters_struck_out", envir = .GlobalEnv)) {
        k_vec <- get("batters_struck_out", envir = .GlobalEnv)
        if (player_id_char %in% names(k_vec)) init_values$K <- k_vec[player_id_char]
      }
      
      if (exists("hr_allowed", envir = .GlobalEnv)) {
        hr_vec <- get("hr_allowed", envir = .GlobalEnv)
        if (player_id_char %in% names(hr_vec)) init_values$HR <- hr_vec[player_id_char]
      }
      
      if (exists("batters_hit", envir = .GlobalEnv)) {
        hbp_vec <- get("batters_hit", envir = .GlobalEnv)
        if (player_id_char %in% names(hbp_vec)) init_values$HBP <- hbp_vec[player_id_char]
      }
    }
    
    tagList(
      div(
        id = entry_id,
        class = "pitcher-entry player-stats-entry",
        style = "padding: 8px; border: 1px solid #ddd; border-radius: 5px; background: #fff; margin-bottom: 8px;",
        
        # Single row with all stats and player selection
        div(style = "display: flex; align-items: center; flex-wrap: nowrap; overflow-x: auto; gap: 5px; margin-bottom: 8px;",
            selectInput(pitcher_id, NULL, choices = player_choices, selected = selected, width = "110px"),
            div(class = 'large-numeric-input', numericInput(paste0(pitcher_id, "_OR"), "Outs", init_values$OR, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(pitcher_id, "_BF"), "BF", init_values$BF, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(pitcher_id, "_H"), "H", init_values$H, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(pitcher_id, "_R"), "R", init_values$R, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(pitcher_id, "_ER"), "ER", init_values$ER, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(pitcher_id, "_BB"), "BB", init_values$BB, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(pitcher_id, "_K"), "K", init_values$K, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(pitcher_id, "_HR"), "HR", init_values$HR, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(pitcher_id, "_HBP"), "HBP", init_values$HBP, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(pitcher_id, "_GB"), "GB", init_values$GB, min = 0, width = "60px")),
            div(class = 'large-numeric-input', numericInput(paste0(pitcher_id, "_WP"), "WP", init_values$WP, min = 0, width = "60px")),
            actionButton(paste0("remove_", entry_id), "", icon = icon("times"), 
                         class = "btn-danger btn-sm", 
                         style = "margin-left: 10px;")
        ),
        
        # Stats display at bottom - now using global calculators
        div(
          style = "margin-top: 5px; padding: 5px; background: #f8f9fa; border-radius: 3px;",
          fluidRow(
            column(6, textOutput(paste0(pitcher_id, "_ERA"))),
            column(6, textOutput(paste0(pitcher_id, "_WHIP")))
          )
        )
      )
    )
  }
  
  # --- 3. PLAYER SELECTION HELPER FUNCTIONS ---
  
  # Get available players for a team, excluding already selected players
  get_team_players <- function(team_id, exclude_ids) {
    if (!is.null(players) && nrow(players) > 0) {
      # Filter players by team
      team_players <- players[players$TeamID == team_id, ]
      
      if (nrow(team_players) > 0) {
        # Create player names
        team_players$PlayerName <- paste(team_players$FirstName, team_players$LastInitial)
        
        # Filter out already selected players
        available_players <- team_players[!(team_players$PlayerID %in% exclude_ids),]
        
        if (nrow(available_players) > 0) {
          return(setNames(available_players$PlayerID, available_players$PlayerName))
        }
      }
    }
    return(c())
  }
  
  # Get team players with option to force-include specific players
  get_team_players_with_force <- function(team_id, exclude_ids, force_include = NULL) {
    if (!is.null(players) && nrow(players) > 0) {
      team_players <- players[players$TeamID == team_id, ]
      if (nrow(team_players) > 0) {
        team_players$PlayerName <- paste(team_players$FirstName, team_players$LastInitial)
        # If force_include is set, remove those from exclude_ids
        if (!is.null(force_include)) {
          exclude_ids <- setdiff(exclude_ids, force_include)
        }
        available_players <- team_players[!(team_players$PlayerID %in% exclude_ids) | (team_players$PlayerID %in% force_include),]
        if (nrow(available_players) > 0) {
          return(setNames(available_players$PlayerID, available_players$PlayerName))
        }
      }
    }
    return(c())
  }
  
  # Get player names from IDs - uses global environment
  get_player_names <- function(ids) {
    if (is.null(ids) || length(ids) == 0) return(character(0))
    # Get the lookup from global player data
    if (exists("players", envir = .GlobalEnv)) {
      player_data <- get("players", envir = .GlobalEnv)
      name_lookup <- setNames(paste(player_data$FirstName, player_data$LastInitial), 
                              as.character(player_data$PlayerID))
      return(name_lookup[as.character(ids)])
    }
    return(character(0))
  }
  
  # Helper for setting names in UI components
  safe_setNames <- function(ids) {
    if (is.null(ids) || length(ids) == 0) return(setNames(character(0), character(0)))
    nms <- get_player_names(ids)
    setNames(ids, nms)
  }
  
  # --- 6. REACTIVE EXPRESSIONS ---
  
  # Team data as reactive to allow updates
  teams_df <- reactive({
    req(teams)
    teams
  })
  
  # Game information based on team selection
  game_info <- reactive({
    req(input$homeTeam, input$awayTeam)
    req(input$homeTeam != "", input$awayTeam != "")
    
    # Filter for incomplete games matching the selected teams
    if (exists("games")) {
      game_data <- games[games$HomeTeamID == input$homeTeam & 
                           games$AwayTeamID == input$awayTeam & 
                           games$IsComplete == 0, ]
      
      if (!is.null(game_data) && nrow(game_data) > 0) {
        return(game_data[1,])
      } else {
        # Find all valid incomplete matchups
        valid_games <- games[games$IsComplete == 0, ]
        if (nrow(valid_games) > 0) {
          valid_list <- paste0(
            "Game ", valid_games$GameID, ": Away Team: ",
            sapply(valid_games$AwayTeamID, function(id) teams$TeamName[teams$TeamID == id]),
            ", Home Team: ",
            paste(sapply(valid_games$HomeTeamID, function(id) teams$TeamName[teams$TeamID == id]), collapse = ", ")
          )
          showNotification(
            paste0(
              "No incomplete game found for this matchup.\n",
              "Valid matchups:\n",
              paste(valid_list, collapse = "\n")
            ),
            type = "error",
            duration = 10
          )
        } else {
          showNotification("No incomplete games remain to be entered!", type = "error", duration = 10)
        }
        return(NULL)
      }
    }
    return(NULL)
  })
  
  # --- 7. INITIALIZE MODULES ---
  
  # Initialize the player card module
  player_card_server(
    "standalone_player_card", 
    get_teams = get_teams,
    get_players = get_players, 
    get_player_stats = get_player_stats,
    get_team_colors = get_team_colors,
    stat_explanations = get_stat_explanations()
  )   
  # Initialize advanced metrics and baseball career lab module
  baseball_lab_server(
    "baseball_lab", 
    players_df = get_players(),
    batting_stats_df = get_batting_stats(),
    pitching_stats_df = get_pitching_stats(),
    league_averages = list(
      League_BA = get("League_BA", envir = .GlobalEnv),
      League_OBP = get("League_OBP", envir = .GlobalEnv),
      League_SLG = get("League_SLG", envir = .GlobalEnv),
      League_OPS = get("League_OPS", envir = .GlobalEnv),
      League_ERA = get("League_ERA", envir = .GlobalEnv),
      League_WHIP = get("League_WHIP", envir = .GlobalEnv)
    )
  )
  
  # League standings module
  league_standings_server("league_standings", list(
    get_games = get_games,
    get_teams = get_teams
  ))
  
  # League leaders module
  league_leaders_server("league_leaders")
  
  # Auto-refresh league leaders when tab is accessed
  observeEvent(input$mainMenu, {
    if (input$mainMenu == "leaders") {
      # Small delay to ensure the UI is fully loaded
      shinyjs::delay(300, {
        shinyjs::click("league_leaders-refreshLeaderboardsTop")
      })
    }
  })
  
  # Team snapshot module
  team_snapshot_server_simple(
    "team_snapshot_simple",
    get_teams = get_teams,
    get_players = get_players,
    get_player_stats = get_player_stats,
    get_team_colors = get_team_colors,
    get_games = get_games
  )
  
  # --- 8. UI RENDERINGS ---
  
  # Display game information
  output$gameDate <- renderText({
    game <- game_info()
    if (!is.null(game)) {
      paste("Game Date:", format(as.Date(game$GameDate), "%m/%d/%Y"))
    } else {
      "No scheduled game found for these teams."
    }
  })
  
  output$gameStatus <- renderText({
    game <- game_info()
    if (is.null(game) || nrow(game) == 0) {
      return("")
    } else if (isTRUE(game$IsComplete[1])) {
      "Status: Game completed"
    } else {
      "Status: Scheduled (not played)"
    }
  })
  
  # Display team names
  output$homeTeamName <- renderText({
    req(input$homeTeam)
    team_info <- teams[teams$TeamID == input$homeTeam, ]
    if (!is.null(team_info) && nrow(team_info) > 0) {
      paste("Team:", team_info$TeamName[1])
    } else {
      ""
    }
  })
  
  output$awayTeamName <- renderText({
    req(input$awayTeam)
    team_info <- teams[teams$TeamID == input$awayTeam, ]
    if (!is.null(team_info) && nrow(team_info) > 0) {
      paste("Team:", team_info$TeamName[1])
    } else {
      ""
    }
  })
  
  # Render game details
  output$gameDetails <- renderUI({
    if (!is.null(games)) {
      DT::dataTableOutput("gamesTable")
    }
  })
  
  output$gamesTable <- renderDT({
    if (!is.null(games)) {
      DT::datatable(
        games %>%
          mutate(
            HomeScore = ifelse(IsComplete == 0, '', HomeScore),
            AwayScore = ifelse(IsComplete == 0, '', AwayScore)
          ) %>%
          rename(Date = GameDate) %>%
          select(Date, HomeTeam, AwayTeam, HomeScore, AwayScore, Status),
        options = list(
          pageLength = 10,
          order = list(list(1, 'asc'))
        )
      )
    } else {
      return(NULL)
    }
  })
  
  # Data status output
  output$connectionStatus <- renderUI({
    if (!is.null(teams) && nrow(teams) > 0) {
      return(HTML("<span style='color:green'>Data loaded successfully!</span>"))
    } else {
      return(HTML("<span style='color:red'>No data available.</span>"))
    }
  })
  
  # Calculate and display computed scores using global RBI values
  output$computedHomeScore <- renderText({
    req(home_batting_order(), home_batters())
    
    # Use form values (not globals) since this is for the current game being edited
    home_rbi <- 0
    for (i in seq_along(home_batting_order())) {
      batter_id <- paste0("homeBatter_", match(home_batting_order()[i], home_batters()))
      if (!is.null(input[[batter_id]])) {
        home_rbi <- home_rbi + as.numeric(input[[paste0(batter_id, "_RBI")]] %||% 0)
      }
    }
    paste(home_rbi)
  })
  
  output$computedAwayScore <- renderText({
    req(away_batting_order(), away_batters())
    
    # Use form values (not globals) since this is for the current game being edited
    away_rbi <- 0
    for (i in seq_along(away_batting_order())) {
      batter_id <- paste0("awayBatter_", match(away_batting_order()[i], away_batters()))
      if (!is.null(input[[batter_id]])) {
        away_rbi <- away_rbi + as.numeric(input[[paste0(batter_id, "_RBI")]] %||% 0)
      }
    }
    paste(away_rbi)
  })
  
  # Render team batting summary using global vectors when possible
  output$battingSummary <- renderUI({
    req(input$homeTeam, input$awayTeam)
    # Also require the reactive values
    req(home_batters(), away_batters())
    
    # Get team names from RDS
    home_team_info <- teams[teams$TeamID == input$homeTeam, ]
    away_team_info <- teams[teams$TeamID == input$awayTeam, ]
    
    # Safely extract team names
    home_team_name <- if (!is.null(home_team_info) && nrow(home_team_info) > 0) 
      home_team_info$TeamName[1] else "Home Team"
    away_team_name <- if (!is.null(away_team_info) && nrow(away_team_info) > 0) 
      away_team_info$TeamName[1] else "Away Team"
    
    # Get stats directly from global vectors where possible
    home_player_ids <- as.character(home_batters())
    away_player_ids <- as.character(away_batters())
    
    # Use isolate for stats calculations
    home_stats <- isolate({
      stats <- list(PA = 0, H = 0, HR = 0, RBI = 0, BB = 0, K = 0, Runs = 0)
      
      # Try to use global vectors
      if (exists("PA", envir = .GlobalEnv)) {
        pa_vec <- get("PA", envir = .GlobalEnv)
        stats$PA <- sum(as.numeric(pa_vec[home_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(home_batters())) {
          batter_id <- paste0("homeBatter_", i)
          if (!is.null(input[[batter_id]])) {
            stats$PA <- stats$PA + as.numeric(input[[paste0(batter_id, "_PA")]] %||% 0)
          }
        }
      }
      
      # Calculate hits
      if (exists("H", envir = .GlobalEnv)) {
        h_vec <- get("H", envir = .GlobalEnv)
        stats$H <- sum(as.numeric(h_vec[home_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(home_batters())) {
          batter_id <- paste0("homeBatter_", i)
          if (!is.null(input[[batter_id]])) {
            stats$H <- stats$H + 
              as.numeric(input[[paste0(batter_id, "_1B")]] %||% 0) + 
              as.numeric(input[[paste0(batter_id, "_2B")]] %||% 0) + 
              as.numeric(input[[paste0(batter_id, "_3B")]] %||% 0) + 
              as.numeric(input[[paste0(batter_id, "_HR")]] %||% 0)
          }
        }
      }
      
      # Get home runs
      if (exists("home_runs", envir = .GlobalEnv)) {
        hr_vec <- get("home_runs", envir = .GlobalEnv)
        stats$HR <- sum(as.numeric(hr_vec[home_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(home_batters())) {
          batter_id <- paste0("homeBatter_", i)
          if (!is.null(input[[batter_id]])) {
            stats$HR <- stats$HR + as.numeric(input[[paste0(batter_id, "_HR")]] %||% 0)
          }
        }
      }
      
      # Get RBIs
      if (exists("rbis", envir = .GlobalEnv)) {
        rbi_vec <- get("rbis", envir = .GlobalEnv)
        stats$RBI <- sum(as.numeric(rbi_vec[home_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(home_batters())) {
          batter_id <- paste0("homeBatter_", i)
          if (!is.null(input[[batter_id]])) {
            stats$RBI <- stats$RBI + as.numeric(input[[paste0(batter_id, "_RBI")]] %||% 0)
          }
        }
      }
      
      # Get walks
      if (exists("walks", envir = .GlobalEnv)) {
        bb_vec <- get("walks", envir = .GlobalEnv)
        stats$BB <- sum(as.numeric(bb_vec[home_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(home_batters())) {
          batter_id <- paste0("homeBatter_", i)
          if (!is.null(input[[batter_id]])) {
            stats$BB <- stats$BB + as.numeric(input[[paste0(batter_id, "_BB")]] %||% 0)
          }
        }
      }
      
      # Get strikeouts
      if (exists("strikeouts", envir = .GlobalEnv)) {
        k_vec <- get("strikeouts", envir = .GlobalEnv)
        stats$K <- sum(as.numeric(k_vec[home_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(home_batters())) {
          batter_id <- paste0("homeBatter_", i)
          if (!is.null(input[[batter_id]])) {
            stats$K <- stats$K + as.numeric(input[[paste0(batter_id, "_K")]] %||% 0)
          }
        }
      }
      
      stats
    })
    
    # Calculate away stats using the same approach
    away_stats <- isolate({
      stats <- list(PA = 0, H = 0, HR = 0, RBI = 0, BB = 0, K = 0, Runs = 0)
      
      # Try to use global vectors
      if (exists("PA", envir = .GlobalEnv)) {
        pa_vec <- get("PA", envir = .GlobalEnv)
        stats$PA <- sum(as.numeric(pa_vec[away_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(away_batters())) {
          batter_id <- paste0("awayBatter_", i)
          if (!is.null(input[[batter_id]])) {
            stats$PA <- stats$PA + as.numeric(input[[paste0(batter_id, "_PA")]] %||% 0)
          }
        }
      }
      
      # Calculate hits
      if (exists("H", envir = .GlobalEnv)) {
        h_vec <- get("H", envir = .GlobalEnv)
        stats$H <- sum(as.numeric(h_vec[away_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(away_batters())) {
          batter_id <- paste0("awayBatter_", i)
          if (!is.null(input[[batter_id]])) {
            stats$H <- stats$H + 
              as.numeric(input[[paste0(batter_id, "_1B")]] %||% 0) + 
              as.numeric(input[[paste0(batter_id, "_2B")]] %||% 0) + 
              as.numeric(input[[paste0(batter_id, "_3B")]] %||% 0) + 
              as.numeric(input[[paste0(batter_id, "_HR")]] %||% 0)
          }
        }
      }
      
      # Get home runs
      if (exists("home_runs", envir = .GlobalEnv)) {
        hr_vec <- get("home_runs", envir = .GlobalEnv)
        stats$HR <- sum(as.numeric(hr_vec[away_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(away_batters())) {
          batter_id <- paste0("awayBatter_", i)
          if (!is.null(input[[batter_id]])) {
            stats$HR <- stats$HR + as.numeric(input[[paste0(batter_id, "_HR")]] %||% 0)
          }
        }
      }
      
      # Get RBIs
      if (exists("rbis", envir = .GlobalEnv)) {
        rbi_vec <- get("rbis", envir = .GlobalEnv)
        stats$RBI <- sum(as.numeric(rbi_vec[away_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(away_batters())) {
          batter_id <- paste0("awayBatter_", i)
          if (!is.null(input[[batter_id]])) {
            stats$RBI <- stats$RBI + as.numeric(input[[paste0(batter_id, "_RBI")]] %||% 0)
          }
        }
      }
      
      # Get walks
      if (exists("walks", envir = .GlobalEnv)) {
        bb_vec <- get("walks", envir = .GlobalEnv)
        stats$BB <- sum(as.numeric(bb_vec[away_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(away_batters())) {
          batter_id <- paste0("awayBatter_", i)
          if (!is.null(input[[batter_id]])) {
            stats$BB <- stats$BB + as.numeric(input[[paste0(batter_id, "_BB")]] %||% 0)
          }
        }
      }
      
      # Get strikeouts
      if (exists("strikeouts", envir = .GlobalEnv)) {
        k_vec <- get("strikeouts", envir = .GlobalEnv)
        stats$K <- sum(as.numeric(k_vec[away_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(away_batters())) {
          batter_id <- paste0("awayBatter_", i)
          if (!is.null(input[[batter_id]])) {
            stats$K <- stats$K + as.numeric(input[[paste0(batter_id, "_K")]] %||% 0)
          }
        }
      }
      
      stats
    })
    
    # Return the summary UI
    fluidRow(
      column(6,
             h4(away_team_name, "(Away)"),
             tags$table(class = "table table-bordered",
                        tags$thead(
                          tags$tr(
                            tags$th("Stat"),
                            tags$th("Value")
                          )
                        ),
                        tags$tbody(
                          tags$tr(tags$td("PA"), tags$td(away_stats$PA)),
                          tags$tr(tags$td("Hits"), tags$td(away_stats$H)),
                          tags$tr(tags$td("HR"), tags$td(away_stats$HR)),
                          tags$tr(tags$td("RBI"), tags$td(away_stats$RBI)),
                          tags$tr(tags$td("BB"), tags$td(away_stats$BB)),
                          tags$tr(tags$td("K"), tags$td(away_stats$K))
                        )
             )
      ),
      column(6,
             h4(home_team_name, "(Home)"),
             tags$table(class = "table table-bordered",
                        tags$thead(
                          tags$tr(
                            tags$th("Stat"),
                            tags$th("Value")
                          )
                        ),
                        tags$tbody(
                          tags$tr(tags$td("PA"), tags$td(home_stats$PA)),
                          tags$tr(tags$td("Hits"), tags$td(home_stats$H)),
                          tags$tr(tags$td("HR"), tags$td(home_stats$HR)),
                          tags$tr(tags$td("RBI"), tags$td(home_stats$RBI)),
                          tags$tr(tags$td("BB"), tags$td(home_stats$BB)),
                          tags$tr(tags$td("K"), tags$td(home_stats$K))
                        )
             )
      )
    )
  })
  
  # Render team pitching summary using global vectors when possible
  output$pitchingSummary <- renderUI({
    req(input$homeTeam, input$awayTeam)
    # Also require the reactive values
    req(home_pitchers(), away_pitchers())
    
    # Get team names from RDS
    home_team_info <- teams[teams$TeamID == input$homeTeam, ]
    away_team_info <- teams[teams$TeamID == input$awayTeam, ]
    
    # Safely extract team names
    home_team_name <- if (!is.null(home_team_info) && nrow(home_team_info) > 0) 
      home_team_info$TeamName[1] else "Home Team"
    away_team_name <- if (!is.null(away_team_info) && nrow(away_team_info) > 0) 
      away_team_info$TeamName[1] else "Away Team"
    
    # Get stats directly from global vectors where possible
    home_player_ids <- as.character(home_pitchers())
    away_player_ids <- as.character(away_pitchers())
    
    # Use isolate for stats calculations
    home_stats <- isolate({
      stats <- list(IP = 0, H = 0, R = 0, ER = 0, BB = 0, K = 0)
      
      # Try to use global vectors for innings pitched
      if (exists("IP", envir = .GlobalEnv)) {
        ip_vec <- get("IP", envir = .GlobalEnv)
        stats$IP <- sum(as.numeric(ip_vec[home_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(home_pitchers())) {
          pitcher_id <- paste0("homePitcher_", i)
          if (!is.null(input[[pitcher_id]])) {
            stats$IP <- stats$IP + as.numeric(input[[paste0(pitcher_id, "_OR")]] %||% 0) / 3
          }
        }
      }
      
      # Get hits allowed
      if (exists("hits_allowed", envir = .GlobalEnv)) {
        h_vec <- get("hits_allowed", envir = .GlobalEnv)
        stats$H <- sum(as.numeric(h_vec[home_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(home_pitchers())) {
          pitcher_id <- paste0("homePitcher_", i)
          if (!is.null(input[[pitcher_id]])) {
            stats$H <- stats$H + as.numeric(input[[paste0(pitcher_id, "_H")]] %||% 0)
          }
        }
      }
      
      # Get runs allowed
      if (exists("runs_allowed", envir = .GlobalEnv)) {
        r_vec <- get("runs_allowed", envir = .GlobalEnv)
        stats$R <- sum(as.numeric(r_vec[home_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(home_pitchers())) {
          pitcher_id <- paste0("homePitcher_", i)
          if (!is.null(input[[pitcher_id]])) {
            stats$R <- stats$R + as.numeric(input[[paste0(pitcher_id, "_R")]] %||% 0)
          }
        }
      }
      
      # Get earned runs
      if (exists("earned_runs", envir = .GlobalEnv)) {
        er_vec <- get("earned_runs", envir = .GlobalEnv)
        stats$ER <- sum(as.numeric(er_vec[home_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(home_pitchers())) {
          pitcher_id <- paste0("homePitcher_", i)
          if (!is.null(input[[pitcher_id]])) {
            stats$ER <- stats$ER + as.numeric(input[[paste0(pitcher_id, "_ER")]] %||% 0)
          }
        }
      }
      
      # Get walks allowed
      if (exists("walks_allowed", envir = .GlobalEnv)) {
        bb_vec <- get("walks_allowed", envir = .GlobalEnv)
        stats$BB <- sum(as.numeric(bb_vec[home_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(home_pitchers())) {
          pitcher_id <- paste0("homePitcher_", i)
          if (!is.null(input[[pitcher_id]])) {
            stats$BB <- stats$BB + as.numeric(input[[paste0(pitcher_id, "_BB")]] %||% 0)
          }
        }
      }
      
      # Get strikeouts
      if (exists("batters_struck_out", envir = .GlobalEnv)) {
        k_vec <- get("batters_struck_out", envir = .GlobalEnv)
        stats$K <- sum(as.numeric(k_vec[home_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(home_pitchers())) {
          pitcher_id <- paste0("homePitcher_", i)
          if (!is.null(input[[pitcher_id]])) {
            stats$K <- stats$K + as.numeric(input[[paste0(pitcher_id, "_K")]] %||% 0)
          }
        }
      }
      
      stats
    })
    
    # Calculate away team pitching stats using the same approach
    away_stats <- isolate({
      stats <- list(IP = 0, H = 0, R = 0, ER = 0, BB = 0, K = 0)
      
      # Try to use global vectors for innings pitched
      if (exists("IP", envir = .GlobalEnv)) {
        ip_vec <- get("IP", envir = .GlobalEnv)
        stats$IP <- sum(as.numeric(ip_vec[away_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(away_pitchers())) {
          pitcher_id <- paste0("awayPitcher_", i)
          if (!is.null(input[[pitcher_id]])) {
            stats$IP <- stats$IP + as.numeric(input[[paste0(pitcher_id, "_OR")]] %||% 0) / 3
          }
        }
      }
      
      # Get hits allowed
      if (exists("hits_allowed", envir = .GlobalEnv)) {
        h_vec <- get("hits_allowed", envir = .GlobalEnv)
        stats$H <- sum(as.numeric(h_vec[away_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(away_pitchers())) {
          pitcher_id <- paste0("awayPitcher_", i)
          if (!is.null(input[[pitcher_id]])) {
            stats$H <- stats$H + as.numeric(input[[paste0(pitcher_id, "_H")]] %||% 0)
          }
        }
      }
      
      # Get runs allowed
      if (exists("runs_allowed", envir = .GlobalEnv)) {
        r_vec <- get("runs_allowed", envir = .GlobalEnv)
        stats$R <- sum(as.numeric(r_vec[away_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(away_pitchers())) {
          pitcher_id <- paste0("awayPitcher_", i)
          if (!is.null(input[[pitcher_id]])) {
            stats$R <- stats$R + as.numeric(input[[paste0(pitcher_id, "_R")]] %||% 0)
          }
        }
      }
      
      # Get earned runs
      if (exists("earned_runs", envir = .GlobalEnv)) {
        er_vec <- get("earned_runs", envir = .GlobalEnv)
        stats$ER <- sum(as.numeric(er_vec[away_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(away_pitchers())) {
          pitcher_id <- paste0("awayPitcher_", i)
          if (!is.null(input[[pitcher_id]])) {
            stats$ER <- stats$ER + as.numeric(input[[paste0(pitcher_id, "_ER")]] %||% 0)
          }
        }
      }
      
      # Get walks allowed
      if (exists("walks_allowed", envir = .GlobalEnv)) {
        bb_vec <- get("walks_allowed", envir = .GlobalEnv)
        stats$BB <- sum(as.numeric(bb_vec[away_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(away_pitchers())) {
          pitcher_id <- paste0("awayPitcher_", i)
          if (!is.null(input[[pitcher_id]])) {
            stats$BB <- stats$BB + as.numeric(input[[paste0(pitcher_id, "_BB")]] %||% 0)
          }
        }
      }
      
      # Get strikeouts
      if (exists("batters_struck_out", envir = .GlobalEnv)) {
        k_vec <- get("batters_struck_out", envir = .GlobalEnv)
        stats$K <- sum(as.numeric(k_vec[away_player_ids]), na.rm=TRUE)
      } else {
        # Fallback to form values
        for (i in seq_along(away_pitchers())) {
          pitcher_id <- paste0("awayPitcher_", i)
          if (!is.null(input[[pitcher_id]])) {
            stats$K <- stats$K + as.numeric(input[[paste0(pitcher_id, "_K")]] %||% 0)
          }
        }
      }
      
      stats
    })
    
    # Calculate derived stats: ERA and WHIP for both teams using safe functions
    # Use safe functions from object_engine.R
    home_era <- if (home_stats$IP > 0) {
      safe_era(home_stats$ER, home_stats$IP)
    } else {
      NA
    }
    
    home_whip <- if (home_stats$IP > 0) {
      safe_whip(home_stats$BB, home_stats$H, 0, home_stats$IP)
    } else {
      NA
    }
    
    away_era <- if (away_stats$IP > 0) {
      safe_era(away_stats$ER, away_stats$IP)
    } else {
      NA
    }
    
    away_whip <- if (away_stats$IP > 0) {
      safe_whip(away_stats$BB, away_stats$H, 0, away_stats$IP)
    } else {
      NA
    }
    
    # Return the summary UI
    fluidRow(
      column(6,
             h4(away_team_name, "(Away)"),
             tags$table(class = "table table-bordered",
                        tags$thead(
                          tags$tr(
                            tags$th("Stat"),
                            tags$th("Value")
                          )
                        ),
                        tags$tbody(
                          tags$tr(tags$td("IP"), tags$td(sprintf("%.1f", away_stats$IP))),
                          tags$tr(tags$td("Hits"), tags$td(away_stats$H)),
                          tags$tr(tags$td("Runs"), tags$td(away_stats$R)),
                          tags$tr(tags$td("ER"), tags$td(away_stats$ER)),
                          tags$tr(tags$td("BB"), tags$td(away_stats$BB)),
                          tags$tr(tags$td("K"), tags$td(away_stats$K)),
                          tags$tr(tags$td("ERA"), tags$td(ifelse(is.na(away_era), "—", sprintf("%.2f", away_era)))),
                          tags$tr(tags$td("WHIP"), tags$td(ifelse(is.na(away_whip), "—", sprintf("%.2f", away_whip))))
                        )
             )
      ),
      column(6,
             h4(home_team_name, "(Home)"),
             tags$table(class = "table table-bordered",
                        tags$thead(
                          tags$tr(
                            tags$th("Stat"),
                            tags$th("Value")
                          )
                        ),
                        tags$tbody(
                          tags$tr(tags$td("IP"), tags$td(sprintf("%.1f", home_stats$IP))),
                          tags$tr(tags$td("Hits"), tags$td(home_stats$H)),
                          tags$tr(tags$td("Runs"), tags$td(home_stats$R)),
                          tags$tr(tags$td("ER"), tags$td(home_stats$ER)),
                          tags$tr(tags$td("BB"), tags$td(home_stats$BB)),
                          tags$tr(tags$td("K"), tags$td(home_stats$K)),
                          tags$tr(tags$td("ERA"), tags$td(ifelse(is.na(home_era), "—", sprintf("%.2f", home_era)))),
                          tags$tr(tags$td("WHIP"), tags$td(ifelse(is.na(home_whip), "—", sprintf("%.2f", home_whip))))
                        )
             )
      )
    )
  })
  
  # --- 9. TEAM SELECTION EVENT HANDLERS ---
  
  # Load teams for dropdowns
  observe({
    if (!is.null(teams) && nrow(teams) > 0) {
      team_choices <- setNames(teams$TeamID, teams$TeamName)
      updateSelectInput(session, "homeTeam", choices = c("Select Team" = "", team_choices))
      updateSelectInput(session, "awayTeam", choices = c("Select Team" = "", team_choices))
      
      # Update filter team dropdown for view/edit games tab
      updateSelectInput(session, "filterTeam", 
                        choices = c("All Teams" = "", team_choices))
    }
  })
  
  # Disable team changes once players are added
  observe({
    if (has_players_added()) {
      disable("homeTeam")
      disable("awayTeam")
    } else {
      enable("homeTeam")
      enable("awayTeam")
    }
  })
  
  # Validate team selection - can't be the same team
  observe({
    if (!is.null(input$homeTeam) && !is.null(input$awayTeam) && 
        input$homeTeam != "" && input$awayTeam != "" && 
        input$homeTeam == input$awayTeam) {
      showNotification("Home and Away teams cannot be the same.", type = "error")
      updateSelectInput(session, "awayTeam", selected = "")
    }
  })
  
  # --- 10. EVENT HANDLERS - FORM RESET ---
  
  # Clear form button handler
  observeEvent(input$clearForm, {
    showModal(modalDialog(
      title = "Confirm Reset",
      "Are you sure you want to clear all form data? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmClear", "Yes, Clear Form", style = "background-color: #dc3545; color: white")
      )
    ))
  })
  
  # Handle clear confirmation
  observeEvent(input$confirmClear, {
    removeModal()
    
    # Reset team selections
    updateSelectInput(session, "homeTeam", selected = "")
    updateSelectInput(session, "awayTeam", selected = "")
    
    # Reset score inputs
    updateNumericInput(session, "homeScore", value = NULL)
    updateNumericInput(session, "awayScore", value = NULL)
    
    # Reset player tracking first (before removing UI elements)
    home_batters(c())
    away_batters(c())
    home_pitchers(c())
    away_pitchers(c())
    
    # Enable team selection
    has_players_added(FALSE)
    
    # Clear the UI containers in a safer way
    tryCatch({
      cat("Resetting UI containers...\n")
      
      # Remove all dynamically added batters and pitchers
      removeUI(selector = "#homeBattingContainer > div")
      removeUI(selector = "#awayBattingContainer > div")
      removeUI(selector = "#homePitchingContainer > div")
      removeUI(selector = "#awayPitchingContainer > div")
      
      # Additional JavaScript to ensure complete removal
      runjs("
    $('#homeBattingContainer').empty();
    $('#awayBattingContainer').empty();
    $('#homePitchingContainer').empty();
    $('#awayPitchingContainer').empty();
  ")
      
      cat("UI reset complete\n")
    }, error = function(ui_error) {
      cat("UI reset error:", ui_error$message, "\n")
    })
  })
  
  # --- 11. EVENT HANDLERS - PLAYER MANAGEMENT ---
  
  # Add Away Batter UI
  observeEvent(input$addAwayBatter, {
    cat('[DEBUG] Add Away Batter button clicked\n')
    has_players_added(TRUE)
    
    ids <- away_batters()
    available <- setdiff(as.character(get_team_players(input$awayTeam, ids)), ids)
    
    if (length(available) == 0) {
      showNotification("All players have been added as batters.", type = "warning")
      return()
    }
    
    new_id <- available[1]
    if (!(new_id %in% ids)) {
      new_ids <- c(ids, new_id)
      away_batters(new_ids)
      
      # Insert UI for new batter
      batter_num <- length(new_ids)
      batter_id <- paste0("awayBatter_", batter_num)
      entry_id <- paste0("away_batting_entry_", batter_num)
      batter_ui <- createBatterUI("away", batter_num, available, selected = new_id)
      insertUI(
        selector = "#awayBattingContainer",
        where = "beforeEnd",
        ui = batter_ui
      )
      
      # Update away_batters reactive to track selected player
      observeEvent(input[[batter_id]], {
        if (!is.null(input[[batter_id]])) {
          # Get current list
          current <- away_batters()
          # Add newly selected player
          away_batters(unique(c(current, input[[batter_id]])))
        }
      }, once = FALSE)
      
      # Calculate and display derived stats using safe functions from object_engine.R
      observe({
        req(input[[batter_id]])
        
        # Robustly parse numeric input, defaulting to 0 if NA or blank
        get_num <- function(x) { v <- suppressWarnings(as.numeric(x)); if (is.na(v)) 0 else v }
        singles <- get_num(input[[paste0(batter_id, "_1B")]])
        doubles <- get_num(input[[paste0(batter_id, "_2B")]])
        triples <- get_num(input[[paste0(batter_id, "_3B")]])
        hr <- get_num(input[[paste0(batter_id, "_HR")]])
        bb <- get_num(input[[paste0(batter_id, "_BB")]])
        hbp <- get_num(input[[paste0(batter_id, "_HBP")]])
        sf <- get_num(input[[paste0(batter_id, "_SF")]])
        pa <- get_num(input[[paste0(batter_id, "_PA")]])
        
        # Calculate stats using safe_* functions from object_engine
        hits <- singles + doubles + triples + hr
        ab <- pa - bb - hbp - sf
        
        # Use safe functions from object_engine
        avg <- safe_batting_avg(hits, ab)
        obp <- safe_obp(hits, bb, hbp, ab, sf)
        
        # Display
        output[[paste0(batter_id, "_AVG")]] <- renderText({
          paste("AVG:", format(avg, nsmall=3))
        })
        
        output[[paste0(batter_id, "_OBP")]] <- renderText({
          paste("OBP:", format(obp, nsmall=3))
        })
      })
      
      # Remove button handler
      observeEvent(input[[paste0("remove_", entry_id)]], {
        # Get the player ID
        player_id <- input[[batter_id]]
        
        if (!is.null(player_id)) {
          # Update the list of used players
          current <- away_batters()
          away_batters(current[current != player_id])
        }
        
        # Remove the UI element
        removeUI(selector = paste0("#", entry_id))
        
        # Re-enable team selection if all players are removed
        if (length(away_batters()) == 0 && length(home_batters()) == 0 && 
            length(away_pitchers()) == 0 && length(home_pitchers()) == 0) {
          has_players_added(FALSE)
        }
      })
    }
  })
  
  # Add Home Batter UI - similar structure to away batter
  observeEvent(input$addHomeBatter, {
    cat('[DEBUG] Add Home Batter button clicked\n')
    has_players_added(TRUE)
    
    ids <- home_batters()
    available <- setdiff(as.character(get_team_players(input$homeTeam, ids)), ids)
    
    if (length(available) == 0) {
      showNotification("All players have been added as batters.", type = "warning")
      return()
    }
    
    new_id <- available[1]
    if (!(new_id %in% ids)) {
      new_ids <- c(ids, new_id)
      home_batters(new_ids)
      
      # Insert UI for new batter
      batter_num <- length(new_ids)
      batter_id <- paste0("homeBatter_", batter_num)
      entry_id <- paste0("home_batting_entry_", batter_num)
      batter_ui <- createBatterUI("home", batter_num, available, selected = new_id)
      insertUI(
        selector = "#homeBattingContainer",
        where = "beforeEnd",
        ui = batter_ui
      )
      
      # Update home_batters reactive to track selected player
      observeEvent(input[[batter_id]], {
        if (!is.null(input[[batter_id]])) {
          # Get current list
          current <- home_batters()
          # Add newly selected player
          home_batters(unique(c(current, input[[batter_id]])))
        }
      }, once = FALSE)
      
      # Calculate and display derived stats using safe functions from object_engine.R
      observe({
        req(input[[batter_id]])
        
        # Robustly parse numeric input, defaulting to 0 if NA or blank
        get_num <- function(x) { v <- suppressWarnings(as.numeric(x)); if (is.na(v)) 0 else v }
        singles <- get_num(input[[paste0(batter_id, "_1B")]])
        doubles <- get_num(input[[paste0(batter_id, "_2B")]])
        triples <- get_num(input[[paste0(batter_id, "_3B")]])
        hr <- get_num(input[[paste0(batter_id, "_HR")]])
        bb <- get_num(input[[paste0(batter_id, "_BB")]])
        hbp <- get_num(input[[paste0(batter_id, "_HBP")]])
        sf <- get_num(input[[paste0(batter_id, "_SF")]])
        pa <- get_num(input[[paste0(batter_id, "_PA")]])
        
        # Calculate stats using safe_* functions from object_engine
        hits <- singles + doubles + triples + hr
        ab <- pa - bb - hbp - sf
        
        # Use safe functions from object_engine
        avg <- safe_batting_avg(hits, ab)
        obp <- safe_obp(hits, bb, hbp, ab, sf)
        
        # Display
        output[[paste0(batter_id, "_AVG")]] <- renderText({
          paste("AVG:", format(avg, nsmall=3))
        })
        
        output[[paste0(batter_id, "_OBP")]] <- renderText({
          paste("OBP:", format(obp, nsmall=3))
        })
      })
      
      # Remove button handler
      observeEvent(input[[paste0("remove_", entry_id)]], {
        # Get the player ID
        player_id <- input[[batter_id]]
        
        if (!is.null(player_id)) {
          # Update the list of used players
          current <- home_batters()
          home_batters(current[current != player_id])
        }
        
        # Remove the UI element
        removeUI(selector = paste0("#", entry_id))
        
        # Re-enable team selection if all players are removed
        if (length(away_batters()) == 0 && length(home_batters()) == 0 && 
            length(away_pitchers()) == 0 && length(home_pitchers()) == 0) {
          has_players_added(FALSE)
        }
      })
    }
  })
  
  # Add Away Pitcher UI - using the same pattern with global functions
  observeEvent(input$addAwayPitcher, {
    cat('[DEBUG] Add Away Pitcher button clicked\n')
    has_players_added(TRUE)
    ids <- away_pitchers()
    available <- setdiff(as.character(get_team_players(input$awayTeam, ids)), ids)
    if (length(available) == 0) {
      showNotification("All players have been added as pitchers.", type = "warning")
      return()
    }
    new_id <- available[1]
    if (!(new_id %in% ids)) {
      new_ids <- c(ids, new_id)
      away_pitchers(new_ids)
      
      # Insert UI for new pitcher
      pitcher_num <- length(new_ids)
      pitcher_id <- paste0("awayPitcher_", pitcher_num)
      entry_id <- paste0("away_pitching_entry_", pitcher_num)
      pitcher_ui <- createPitcherUI("away", pitcher_num, available, selected = new_id)
      insertUI(
        selector = "#awayPitchingContainer",
        where = "beforeEnd",
        ui = pitcher_ui
      )
      
      # Update away_pitchers reactive to track selected player
      observeEvent(input[[pitcher_id]], {
        if (!is.null(input[[pitcher_id]])) {
          current <- away_pitchers()
          away_pitchers(unique(c(current, input[[pitcher_id]])))
        }
      }, once = FALSE)
      
      # Calculate and display derived stats using safe functions from object_engine.R
      observe({
        req(input[[pitcher_id]])
        
        # Robustly parse numeric input, defaulting to 0 if NA or blank
        get_num <- function(x) { v <- suppressWarnings(as.numeric(x)); if (is.na(v)) 0 else v }
        outs <- get_num(input[[paste0(pitcher_id, "_OR")]])
        h <- get_num(input[[paste0(pitcher_id, "_H")]])
        bb <- get_num(input[[paste0(pitcher_id, "_BB")]])
        er <- get_num(input[[paste0(pitcher_id, "_ER")]])
        hbp <- get_num(input[[paste0(pitcher_id, "_HBP")]])
        
        # Use safe functions from object_engine
        ip <- outs / 3
        era <- safe_era(er, ip)
        whip <- safe_whip(bb, h, hbp, ip)
        
        # Display
        output[[paste0(pitcher_id, "_ERA")]] <- renderText({ 
          paste("ERA:", format(era, nsmall=2))
        })
        
        output[[paste0(pitcher_id, "_WHIP")]] <- renderText({ 
          paste("WHIP:", format(whip, nsmall=2))
        })
      })
      
      # Remove button handler
      observeEvent(input[[paste0("remove_", entry_id)]], {
        player_id <- input[[pitcher_id]]
        if (!is.null(player_id)) {
          current <- away_pitchers()
          away_pitchers(current[current != player_id])
        }
        removeUI(selector = paste0("#", entry_id))
        if (length(away_batters()) == 0 && length(home_batters()) == 0 && 
            length(away_pitchers()) == 0 && length(home_pitchers()) == 0) {
          has_players_added(FALSE)
        }
      })
    }
  })
  
  # Add Home Pitcher UI - using the same pattern with global functions
  observeEvent(input$addHomePitcher, {
    cat('[DEBUG] Add Home Pitcher button clicked\n')
    has_players_added(TRUE)
    
    ids <- home_pitchers()
    available <- setdiff(as.character(get_team_players(input$homeTeam, ids)), ids)
    
    if (length(available) == 0) {
      showNotification("All players have been added as pitchers.", type = "warning")
      return()
    }
    
    new_id <- available[1]
    if (!(new_id %in% ids)) {
      new_ids <- c(ids, new_id)
      home_pitchers(new_ids)
      
      # Force update the pitching order with the new player added
      current_order <- home_pitching_order()
      home_pitching_order(c(current_order, new_id))
      
      # Update global labels for the sortable interface
      observe({
        .home_pitching_order_labels <<- get_player_names(home_pitching_order())
      })
      
      # Pitcher UI logic (must be inside this block)
      pitcher_num <- length(new_ids)
      pitcher_id <- paste0("homePitcher_", pitcher_num)
      entry_id <- paste0("home_pitching_entry_", pitcher_num)
      
      # Insert UI
      pitcher_ui <- createPitcherUI("home", pitcher_num, available, selected = new_id)
      insertUI(
        selector = "#homePitchingContainer",
        where = "beforeEnd",
        ui = pitcher_ui
      )
      
      # Update home_pitchers reactive to track selected player
      observeEvent(input[[pitcher_id]], {
        if (!is.null(input[[pitcher_id]])) {
          current <- home_pitchers()
          home_pitchers(unique(c(current, input[[pitcher_id]])))
        }
      }, once = FALSE)
      
      # Calculate and display derived stats using safe functions from object_engine.R
      observe({
        req(input[[pitcher_id]])
        
        # Robustly parse numeric input, defaulting to 0 if NA or blank
        get_num <- function(x) { v <- suppressWarnings(as.numeric(x)); if (is.na(v)) 0 else v }
        outs <- get_num(input[[paste0(pitcher_id, "_OR")]])
        h <- get_num(input[[paste0(pitcher_id, "_H")]])
        bb <- get_num(input[[paste0(pitcher_id, "_BB")]])
        er <- get_num(input[[paste0(pitcher_id, "_ER")]])
        hbp <- get_num(input[[paste0(pitcher_id, "_HBP")]])
        
        # Use safe functions from object_engine
        ip <- outs / 3
        era <- safe_era(er, ip)
        whip <- safe_whip(bb, h, hbp, ip)
        
        # Display
        output[[paste0(pitcher_id, "_ERA")]] <- renderText({
          paste("ERA:", format(era, nsmall=2))
        })
        
        output[[paste0(pitcher_id, "_WHIP")]] <- renderText({
          paste("WHIP:", format(whip, nsmall=2))
        })
      })
      
      # Remove button handler
      observeEvent(input[[paste0("remove_", entry_id)]], {
        # Get the player ID
        player_id <- input[[pitcher_id]]
        
        if (!is.null(player_id)) {
          # Update the list of used players
          current <- home_pitchers()
          home_pitchers(current[current != player_id])
        }
        
        # Remove the UI element
        removeUI(selector = paste0("#", entry_id))
        
        # Re-enable team selection if all players are removed
        if (length(away_batters()) == 0 && length(home_batters()) == 0 && 
            length(away_pitchers()) == 0 && length(home_pitchers()) == 0) {
          has_players_added(FALSE)
        }
      })
    }
  })
  
  # --- 12. GAME DATA MANAGEMENT ---
  
  # Function to update completed games list
  update_completed_games <- function() {
    if (exists("games")) {
      completed_games <- games[games$IsComplete == 1, ]
      
      if (nrow(completed_games) > 0) {
        # Lookup function for team names
        getTeamName <- function(id) {
          match <- teams[teams$TeamID == id, "TeamName"]
          if (length(match) > 0) return(match[1])
          return(paste("Team", id))
        }
        
        # Format the games for the dropdown
        game_labels <- sapply(1:nrow(completed_games), function(i) {
          g <- completed_games[i, ]
          date_str <- format(as.Date(g$GameDate), "%m/%d/%Y")
          home_team <- getTeamName(g$HomeTeamID)
          away_team <- getTeamName(g$AwayTeamID)
          return(paste0(date_str, " - ", away_team, " (", g$AwayScore, ") @ ", home_team, " (", g$HomeScore, ")"))
        })
        
        # Update the dropdown
        game_choices <- setNames(completed_games$GameID, game_labels)
        updateSelectInput(session, "completedGame", choices = c("Select Game" = "", game_choices))
      }
    }
  }
  
  # Database save button handler
  observeEvent(input$saveDatabase, {
    # First, validate if we have required data
    if(is.null(input$homeTeam) || input$homeTeam == "" || 
       is.null(input$awayTeam) || input$awayTeam == "") {
      showNotification("Please select both home and away teams before saving.", type = "error")
      return()
    }
    
    # Check if we have any players added
    if(length(home_batters()) == 0 && length(away_batters()) == 0 && 
       length(home_pitchers()) == 0 && length(away_pitchers()) == 0) {
      showNotification("Please add at least one player statistic before saving.", type = "error")
      return()
    }
    
    # Show confirmation modal
    showModal(modalDialog(
      title = "Confirm Database Entry",
      div(
        p("Are you sure you want to save this game data to the database?"),
        p("This will:"),
        tags$ul(
          tags$li("Save all player statistics"),
          tags$li("Update the game record"),
          tags$li("Mark the game as complete"),
          tags$li("Clear the form for a new game entry")
        ),
        p("This action cannot be undone.")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmSave", "Yes, Save Game Data", 
                     class = "btn-success",
                     icon = icon("check"))
      )
    ))
  })
  
  # Handle save confirmation with improved error handling and detailed logging
  observeEvent(input$confirmSave, {
    removeModal()
    
    # Get game ID
    game <- game_info()
    if (is.null(game)) {
      showNotification("No game found for these teams.", type = "error")
      return()
    }
    
    # Ensure game_id is numeric
    game_id <- as.integer(game$GameID)
    cat("Game ID:", game_id, "\n")
    
    # Calculate HomeScore and AwayScore from RBI stats
    home_runs <- 0
    for (i in seq_along(home_batters())) {
      batter_id <- paste0("homeBatter_", i)
      if (!is.null(input[[batter_id]])) {
        home_runs <- home_runs + as.numeric(input[[paste0(batter_id, "_RBI")]] %||% 0)
      }
    }
    away_runs <- 0
    for (i in seq_along(away_batters())) {
      batter_id <- paste0("awayBatter_", i)
      if (!is.null(input[[batter_id]])) {
        away_runs <- away_runs + as.numeric(input[[paste0(batter_id, "_RBI")]] %||% 0)
      }
    }
    
    # Create withProgress to show user the saving is happening
    withProgress(message = 'Saving game data...', value = 0, {
      
      # Update game record with scores and mark as complete
      incProgress(0.1, detail = "Updating game record...")
      
      # Flag to track save success
      save_success <- FALSE
      
      tryCatch({
        # Update game record in games data frame
        games[games$GameID == game_id, "HomeScore"] <- home_runs
        games[games$GameID == game_id, "AwayScore"] <- away_runs
        
        # Mark as complete
        games[games$GameID == game_id, "IsComplete"] <- 1
        
        # Clear existing stats
        incProgress(0.2, detail = "Clearing existing stats...")
        
        # Remove existing stats for this game
        batting_stats <- batting_stats[batting_stats$GameID != game_id, ]
        pitching_stats <- pitching_stats[pitching_stats$GameID != game_id, ]
        
        # Update steps with progress
        incProgress(0.3, detail = "Saving batting stats...")
        
        # Save home batting stats
        for (i in seq_along(home_batters())) {
          batter_id <- paste0("homeBatter_", i)
          if (!is.null(input[[batter_id]])) {
            # Create new batting stat record
            new_stat <- data.frame(
              GameID = game_id,
              PlayerID = input[[batter_id]],
              PA = as.numeric(input[[paste0(batter_id, "_PA")]] %||% 0),
              X1B = as.numeric(input[[paste0(batter_id, "_1B")]] %||% 0),
              X2B = as.numeric(input[[paste0(batter_id, "_2B")]] %||% 0),
              X3B = as.numeric(input[[paste0(batter_id, "_3B")]] %||% 0),
              HR = as.numeric(input[[paste0(batter_id, "_HR")]] %||% 0),
              RBI = as.numeric(input[[paste0(batter_id, "_RBI")]] %||% 0),
              BB = as.numeric(input[[paste0(batter_id, "_BB")]] %||% 0),
              HBP = as.numeric(input[[paste0(batter_id, "_HBP")]] %||% 0),
              SF = as.numeric(input[[paste0(batter_id, "_SF")]] %||% 0),
              SH = as.numeric(input[[paste0(batter_id, "_SH")]] %||% 0),
              K = as.numeric(input[[paste0(batter_id, "_K")]] %||% 0),
              GIDP = as.numeric(input[[paste0(batter_id, "_GIDP")]] %||% 0),
              SB = as.numeric(input[[paste0(batter_id, "_SB")]] %||% 0),
              CS = as.numeric(input[[paste0(batter_id, "_CS")]] %||% 0),
              ROE = as.numeric(input[[paste0(batter_id, "_ROE")]] %||% 0)
            )
            # Align columns and order for batting_stats
            if (!identical(names(batting_stats), names(new_stat))) {
              cat('Batting stats names mismatch:\n')
              print(names(batting_stats))
              print(names(new_stat))
              missing_cols <- setdiff(names(batting_stats), names(new_stat))
              for (col in missing_cols) new_stat[[col]] <- NA
              new_stat <- new_stat[names(batting_stats)]
            }
            batting_stats <- rbind(batting_stats, new_stat)
          }
        }
        
        # Save away batting stats
        for (i in seq_along(away_batters())) {
          batter_id <- paste0("awayBatter_", i)
          if (!is.null(input[[batter_id]])) {
            # Create new batting stat record
            new_stat <- data.frame(
              GameID = game_id,
              PlayerID = input[[batter_id]],
              PA = as.numeric(input[[paste0(batter_id, "_PA")]] %||% 0),
              X1B = as.numeric(input[[paste0(batter_id, "_1B")]] %||% 0),
              X2B = as.numeric(input[[paste0(batter_id, "_2B")]] %||% 0),
              X3B = as.numeric(input[[paste0(batter_id, "_3B")]] %||% 0),
              HR = as.numeric(input[[paste0(batter_id, "_HR")]] %||% 0),
              RBI = as.numeric(input[[paste0(batter_id, "_RBI")]] %||% 0),
              BB = as.numeric(input[[paste0(batter_id, "_BB")]] %||% 0),
              HBP = as.numeric(input[[paste0(batter_id, "_HBP")]] %||% 0),
              SF = as.numeric(input[[paste0(batter_id, "_SF")]] %||% 0),
              SH = as.numeric(input[[paste0(batter_id, "_SH")]] %||% 0),
              K = as.numeric(input[[paste0(batter_id, "_K")]] %||% 0),
              GIDP = as.numeric(input[[paste0(batter_id, "_GIDP")]] %||% 0),
              SB = as.numeric(input[[paste0(batter_id, "_SB")]] %||% 0),
              CS = as.numeric(input[[paste0(batter_id, "_CS")]] %||% 0),
              ROE = as.numeric(input[[paste0(batter_id, "_ROE")]] %||% 0)
            )
            # Align columns and order for batting_stats
            if (!identical(names(batting_stats), names(new_stat))) {
              cat('Batting stats names mismatch:\n')
              print(names(batting_stats))
              print(names(new_stat))
              missing_cols <- setdiff(names(batting_stats), names(new_stat))
              for (col in missing_cols) new_stat[[col]] <- NA
              new_stat <- new_stat[names(batting_stats)]
            }
            batting_stats <- rbind(batting_stats, new_stat)
          }
        }
        
        # Update steps with progress
        incProgress(0.6, detail = "Saving pitching stats...")
        
        # Save home pitching stats
        for (i in seq_along(home_pitchers())) {
          pitcher_id <- paste0("homePitcher_", i)
          if (!is.null(input[[pitcher_id]])) {
            # Create new pitching stat record
            new_stat <- data.frame(
              GameID = game_id,
              PlayerID = input[[pitcher_id]],
              BF = as.numeric(input[[paste0(pitcher_id, "_BF")]] %||% 0),
              H = as.numeric(input[[paste0(pitcher_id, "_H")]] %||% 0),
              R = as.numeric(input[[paste0(pitcher_id, "_R")]] %||% 0),
              ER = as.numeric(input[[paste0(pitcher_id, "_ER")]] %||% 0),
              BB = as.numeric(input[[paste0(pitcher_id, "_BB")]] %||% 0),
              K = as.numeric(input[[paste0(pitcher_id, "_K")]] %||% 0),
              HBP = as.numeric(input[[paste0(pitcher_id, "_HBP")]] %||% 0),
              HR = as.numeric(input[[paste0(pitcher_id, "_HR")]] %||% 0),
              GB = as.numeric(input[[paste0(pitcher_id, "_GB")]] %||% 0),
              WP = as.numeric(input[[paste0(pitcher_id, "_WP")]] %||% 0),
              OutsRecorded = as.numeric(input[[paste0(pitcher_id, "_OR")]] %||% 0)
            )
            # Align columns and order for pitching_stats
            if (!identical(names(pitching_stats), names(new_stat))) {
              cat('Pitching stats names mismatch:\n')
              print(names(pitching_stats))
              print(names(new_stat))
              missing_cols <- setdiff(names(pitching_stats), names(new_stat))
              for (col in missing_cols) new_stat[[col]] <- NA
              new_stat <- new_stat[names(pitching_stats)]
            }
            pitching_stats <- rbind(pitching_stats, new_stat)
          }
        }
        
        # Save away pitching stats
        for (i in seq_along(away_pitchers())) {
          pitcher_id <- paste0("awayPitcher_", i)
          if (!is.null(input[[pitcher_id]])) {
            # Create new pitching stat record
            new_stat <- data.frame(
              GameID = game_id,
              PlayerID = input[[pitcher_id]],
              BF = as.numeric(input[[paste0(pitcher_id, "_BF")]] %||% 0),
              H = as.numeric(input[[paste0(pitcher_id, "_H")]] %||% 0),
              R = as.numeric(input[[paste0(pitcher_id, "_R")]] %||% 0),
              ER = as.numeric(input[[paste0(pitcher_id, "_ER")]] %||% 0),
              BB = as.numeric(input[[paste0(pitcher_id, "_BB")]] %||% 0),
              K = as.numeric(input[[paste0(pitcher_id, "_K")]] %||% 0),
              HBP = as.numeric(input[[paste0(pitcher_id, "_HBP")]] %||% 0),
              HR = as.numeric(input[[paste0(pitcher_id, "_HR")]] %||% 0),
              GB = as.numeric(input[[paste0(pitcher_id, "_GB")]] %||% 0),
              WP = as.numeric(input[[paste0(pitcher_id, "_WP")]] %||% 0),
              OutsRecorded = as.numeric(input[[paste0(pitcher_id, "_OR")]] %||% 0)
            )
            # Align columns and order for pitching_stats
            if (!identical(names(pitching_stats), names(new_stat))) {
              cat('Pitching stats names mismatch:\n')
              print(names(pitching_stats))
              print(names(new_stat))
              missing_cols <- setdiff(names(pitching_stats), names(new_stat))
              for (col in missing_cols) new_stat[[col]] <- NA
              new_stat <- new_stat[names(pitching_stats)]
            }
            pitching_stats <- rbind(pitching_stats, new_stat)
          }
        }
        
        # Save to RDS files
        saveRDS(games, "tblGames.rds")
        saveRDS(batting_stats, "tblBattingStatsFlat.rds")
        saveRDS(pitching_stats, "tblPitchingStatsFlat.rds")
        
        source("R/object_engine.R")
        save_success <- TRUE
        
      }, error = function(e) {
        cat("Error in save operation:", e$message, "\n")
        showNotification(paste("Error saving game data:", e$message), type = "error")
      })
      
      # If successful, reset the form
      if (save_success) {
        cat("Save successful, resetting form...\n")
        
        # Reset team selections
        updateSelectInput(session, "homeTeam", selected = "")
        updateSelectInput(session, "awayTeam", selected = "")
        
        # Reset player tracking first (before removing UI elements)
        home_batters(c())
        away_batters(c())
        home_pitchers(c())
        away_pitchers(c())
        
        # Enable team selection
        has_players_added(FALSE)
        
        # Clear the UI containers in a safer way
        tryCatch({
          cat("Resetting UI containers...\n")
          
          # Remove all dynamically added batters and pitchers
          removeUI(selector = "#homeBattingContainer > div")
          removeUI(selector = "#awayBattingContainer > div")
          removeUI(selector = "#homePitchingContainer > div")
          removeUI(selector = "#awayPitchingContainer > div")
          
          # Additional JavaScript to ensure complete removal
          runjs("
        $('#homeBattingContainer').empty();
        $('#awayBattingContainer').empty();
        $('#homePitchingContainer').empty();
        $('#awayPitchingContainer').empty();
      ")
          
          cat("UI reset complete\n")
        }, error = function(ui_error) {
          cat("UI reset error:", ui_error$message, "\n")
        })
        
        # Show success message
        showNotification("Game data saved successfully!", type = "message")
        
        # Update the completed games list
        update_completed_games()
        
      }
    })
  })
  
  # --- 13. LEAGUE STANDINGS 
  
  # Create data accessor functions in global environment
  assign("get_games", get_games, envir = .GlobalEnv)
  assign("get_teams", get_teams, envir = .GlobalEnv)
  
  # Initialize league standings module
  league_standings_server("league_standings", data = list(
    get_games = get_games,
    get_teams = get_teams
  ))
  
  # Initialize power rankings module
  power_rankings_server("power_rankings", data = list(
    get_games = get_games,
    get_teams = get_teams
  ))
  
  # --- 13. VIEW/EDIT GAME FUNCTIONALITY ---
  
  # View Game button handler
  observeEvent(input$viewGame, {
    req(input$completedGame)
    
    if (input$completedGame == "") {
      showNotification("Please select a game to view.", type = "warning")
      return()
    }
    
    # Load game details
    game_id <- input$completedGame
    
    # Get complete game details
    game_details <- tryCatch({
      # Get game details from RDS files
      if (exists("games")) {
        game <- games[games$GameID == game_id, ]
        
        if (nrow(game) > 0) {
          # Get team names
          home_team <- teams[teams$TeamID == game$HomeTeamID, ]
          away_team <- teams[teams$TeamID == game$AwayTeamID, ]
          
          # Get batting stats for this game
          batting <- batting_stats[batting_stats$GameID == game_id, ]
          if (nrow(batting) > 0) {
            batting <- merge(batting, players, by = "PlayerID")
            batting$PlayerName <- paste(batting$FirstName, batting$LastInitial)
            batting$TeamName <- ifelse(batting$TeamID == game$HomeTeamID, 
                                       home_team$TeamName, away_team$TeamName)
          }
          
          # Get pitching stats for this game
          pitching <- pitching_stats[pitching_stats$GameID == game_id, ]
          if (nrow(pitching) > 0) {
            pitching <- merge(pitching, players, by = "PlayerID")
            pitching$PlayerName <- paste(pitching$FirstName, pitching$LastInitial)
            pitching$TeamName <- ifelse(pitching$TeamID == game$HomeTeamID, 
                                        home_team$TeamName, away_team$TeamName)
          }
          
          # Add team names to game data
          game$HomeName <- home_team$TeamName
          game$AwayName <- away_team$TeamName
          
          list(game = game, batting = batting, pitching = pitching)
        } else {
          NULL
        }
      } else {
        NULL
      }
    }, error = function(e) {
      cat("Error loading game details:", e$message, "\n")
      NULL
    })
    
    if (is.null(game_details) || nrow(game_details$game) == 0) {
      showNotification("Could not load game data.", type = "error")
      return()
    }
    
    # Store the loaded data for potential editing
    current_game_details(game_details)
    
    # Show the game details
    showModal(
      modalDialog(
        title = paste("Game Details -", format(as.Date(game_details$game$GameDate), "%m/%d/%Y")),
        size = "l",
        
        # Game summary
        div(
          class = "game-summary",
          h4("Final Score"),
          fluidRow(
            column(5, align = "center",
                   h3(game_details$game$AwayName),
                   h2(game_details$game$AwayScore)
            ),
            column(2, align = "center",
                   h3("@")
            ),
            column(5, align = "center",
                   h3(game_details$game$HomeName),
                   h2(game_details$game$HomeScore)
            )
          ),
          hr()
        ),
        
        # Tabset for batting and pitching stats
        tabsetPanel(
          id = "viewStatsTabset",
          
          # Batting stats tab
          tabPanel(
            "Batting Stats",
            fluidRow(
              column(12,
                     selectInput("viewBattingTeam", "Filter by Team:", 
                                 choices = c("All Teams" = "all", 
                                             "Home" = "home", 
                                             "Away" = "away")),
                     DT::dataTableOutput("viewBattingTable"),
                     br(),
                     downloadButton("downloadBatting", "Download CSV")
              )
            )
          ),
          
          # Pitching stats tab
          tabPanel(
            "Pitching Stats",
            fluidRow(
              column(12,
                     selectInput("viewPitchingTeam", "Filter by Team:", 
                                 choices = c("All Teams" = "all", 
                                             "Home" = "home", 
                                             "Away" = "away")),
                     DT::dataTableOutput("viewPitchingTable"),
                     br(),
                     downloadButton("downloadPitching", "Download CSV")
              )
            )
          )
        ),
        
        footer = modalButton("Close")
      )
    )
    
    # Render batting stats table using global functions
    output$viewBattingTable <- DT::renderDataTable({
      batting <- current_game_details()$batting
      game <- current_game_details()$game
      
      # Use player_ids to get stats from global vectors
      player_ids_char <- as.character(batting$PlayerID)
      
      # Add calculated columns using global vectors when available
      if (exists("H", envir = .GlobalEnv)) {
        h_vec <- get("H", envir = .GlobalEnv)
        batting$H <- as.numeric(h_vec[player_ids_char])
      } else {
        batting$H <- batting$X1B + batting$X2B + batting$X3B + batting$HR
      }
      
      if (exists("AB", envir = .GlobalEnv)) {
        ab_vec <- get("AB", envir = .GlobalEnv)
        batting$AB <- as.numeric(ab_vec[player_ids_char])
      } else {
        batting$AB <- batting$PA - batting$BB - batting$HBP - batting$SF - batting$SH
      }
      
      # Use global AVG vector if available
      if (exists("AVG", envir = .GlobalEnv)) {
        avg_vec <- get("AVG", envir = .GlobalEnv)
        batting$AVG <- as.numeric(avg_vec[player_ids_char])
      } else {
        # Use safe functions from object_engine.R
        batting$AVG <- sapply(1:nrow(batting), function(i) {
          safe_batting_avg(batting$H[i], batting$AB[i])
        })
      }
      
      # Use global OBP vector if available
      if (exists("OBP", envir = .GlobalEnv)) {
        obp_vec <- get("OBP", envir = .GlobalEnv)
        batting$OBP <- as.numeric(obp_vec[player_ids_char])
      } else {
        # Use safe functions from object_engine.R
        batting$OBP <- sapply(1:nrow(batting), function(i) {
          safe_obp(batting$H[i], batting$BB[i], batting$HBP[i], batting$AB[i], batting$SF[i])
        })
      }
      
      # Use global SLG vector if available
      if (exists("SLG", envir = .GlobalEnv)) {
        slg_vec <- get("SLG", envir = .GlobalEnv)
        batting$SLG <- as.numeric(slg_vec[player_ids_char])
      } else {
        # Use safe functions from object_engine.R
        batting$SLG <- sapply(1:nrow(batting), function(i) {
          total_bases <- batting$X1B[i] + (2 * batting$X2B[i]) + (3 * batting$X3B[i]) + (4 * batting$HR[i])
          safe_slg(total_bases, batting$AB[i])
        })
      }
      
      # Filter by team if selected
      if (!is.null(input$viewBattingTeam) && input$viewBattingTeam != "all") {
        team_name <- if(input$viewBattingTeam == "home") game$HomeName else game$AwayName
        batting <- batting[batting$TeamName == team_name, ]
      }
      
      # Select columns for display
      display_cols <- c("PlayerName", "TeamName", "AB", "H", "X1B", "X2B", "X3B", "HR", 
                        "RBI", "BB", "K", "AVG", "OBP", "SLG")
      
      DT::datatable(
        batting[, display_cols],
        options = list(
          pageLength = 15,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })
    
    # Render pitching stats table using global functions
    output$viewPitchingTable <- DT::renderDataTable({
      pitching <- current_game_details()$pitching
      game <- current_game_details()$game
      
      # Use player_ids to get stats from global vectors
      player_ids_char <- as.character(pitching$PlayerID)
      
      # Add calculated columns using global vectors when available
      if (exists("IP", envir = .GlobalEnv)) {
        ip_vec <- get("IP", envir = .GlobalEnv)
        pitching$IP <- as.numeric(ip_vec[player_ids_char])
      } else {
        pitching$IP <- pitching$OutsRecorded / 3
      }
      
      # Use global ERA vector if available
      if (exists("ERA", envir = .GlobalEnv)) {
        era_vec <- get("ERA", envir = .GlobalEnv)
        pitching$ERA <- as.numeric(era_vec[player_ids_char])
      } else {
        # Use safe functions from object_engine.R
        pitching$ERA <- sapply(1:nrow(pitching), function(i) {
          safe_era(pitching$ER[i], pitching$IP[i])
        })
      }
      
      # Use global WHIP vector if available
      if (exists("WHIP", envir = .GlobalEnv)) {
        whip_vec <- get("WHIP", envir = .GlobalEnv)
        pitching$WHIP <- as.numeric(whip_vec[player_ids_char])
      } else {
        # Use safe functions from object_engine.R
        pitching$WHIP <- sapply(1:nrow(pitching), function(i) {
          safe_whip(pitching$BB[i], pitching$H[i], pitching$HBP[i], pitching$IP[i])
        })
      }
      
      # Calculate K/9 using global function if available
      if (exists("K_per_9", envir = .GlobalEnv)) {
        k9_vec <- get("K_per_9", envir = .GlobalEnv)
        pitching$K_per_9 <- as.numeric(k9_vec[player_ids_char])
      } else {
        # Use safe functions from object_engine.R
        pitching$K_per_9 <- sapply(1:nrow(pitching), function(i) {
          safe_per_nine(pitching$K[i], pitching$IP[i])
        })
      }
      
      # Format IP for display in the traditional baseball format (IP.1 for 1/3 inning, etc.)
      pitching$IP_Display <- floor(pitching$OutsRecorded / 3) + (pitching$OutsRecorded %% 3) / 10
      
      # Filter by team if selected
      if (!is.null(input$viewPitchingTeam) && input$viewPitchingTeam != "all") {
        team_name <- if(input$viewPitchingTeam == "home") game$HomeName else game$AwayName
        pitching <- pitching[pitching$TeamName == team_name, ]
      }
      
      # Select columns for display
      display_cols <- c("PlayerName", "TeamName", "IP_Display", "H", "R", "ER", 
                        "BB", "K", "HR", "ERA", "WHIP", "K_per_9")
      
      DT::datatable(
        pitching[, display_cols],
        options = list(
          pageLength = 15,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })
  })
  
  # Edit Game button handler
  observeEvent(input$editGame, {
    req(input$completedGame)
    
    if (input$completedGame == "") {
      showNotification("Please select a game to edit.", type = "warning")
      return()
    }
    
    game_id <- input$completedGame
    
    # First, load the game and all related data
    game_details <- tryCatch({
      if (exists("games")) {
        game <- games[games$GameID == game_id, ]
        
        if (nrow(game) > 0) {
          # Get team names
          home_team <- teams[teams$TeamID == game$HomeTeamID, ]
          away_team <- teams[teams$TeamID == game$AwayTeamID, ]
          
          # Get batting stats for this game
          batting <- batting_stats[batting_stats$GameID == game_id, ]
          if (nrow(batting) > 0) {
            batting <- merge(batting, players, by = "PlayerID")
            batting$PlayerName <- paste(batting$FirstName, batting$LastInitial)
            batting$TeamName <- ifelse(batting$TeamID == game$HomeTeamID, 
                                       home_team$TeamName, away_team$TeamName)
          }
          
          # Get pitching stats for this game
          pitching <- pitching_stats[pitching_stats$GameID == game_id, ]
          if (nrow(pitching) > 0) {
            pitching <- merge(pitching, players, by = "PlayerID")
            pitching$TeamName <- ifelse(batting$TeamID == game$HomeTeamID, 
                                        home_team$TeamName, away_team$TeamName)
          }
          
          # Add team names to game data
          game$HomeName <- home_team$TeamName
          game$AwayName <- away_team$TeamName
          
          list(game = game, batting = batting, pitching = pitching)
        } else {
          NULL
        }
      } else {
        NULL
      }
    }, error = function(e) {
      cat("Error loading game details for editing:", e$message, "\n")
      NULL
    })
    
    if (is.null(game_details) || nrow(game_details$game) == 0) {
      showNotification("Could not load game data for editing.", type = "error")
      return()
    }
    
    # Store the loaded data for editing session
    current_game_details(game_details)
    
    # Show edit dialog
    showModal(
      modalDialog(
        title = paste("Edit Game -", format(as.Date(game_details$game$GameDate), "%Y-%m-%d")),
        size = "l",
        
        # Basic game info editing
        fluidRow(
          column(6,
                 textInput("edit_gameDate", "Game Date:", 
                           value = format(as.Date(game_details$game$GameDate), "%Y-%m-%d"))
          ),
          column(3,
                 numericInput("edit_homeScore", "Home Score:", 
                              value = game_details$game$HomeScore)
          ),
          column(3,
                 numericInput("edit_awayScore", "Away Score:", 
                              value = game_details$game$AwayScore)
          )
        ),
        
        fluidRow(
          column(6,
                 h4(game_details$game$HomeName, "(Home)")
          ),
          column(6,
                 h4(game_details$game$AwayName, "(Away)")
          )
        ),
        
        # Tabset for batting and pitching stats
        tabsetPanel(
          id = "editStatsTabset",
          
          # Batting stats tab
          tabPanel(
            "Batting Stats",
            fluidRow(
              column(12,
                     selectInput("edit_battingTeam", "Filter by Team:", 
                                 choices = c("All Teams" = "all", 
                                             "Home" = "home", 
                                             "Away" = "away")),
                     DT::dataTableOutput("edit_battingTable")
              )
            )
          ),
          
          # Pitching stats tab
          tabPanel(
            "Pitching Stats",
            fluidRow(
              column(12,
                     selectInput("edit_pitchingTeam", "Filter by Team:", 
                                 choices = c("All Teams" = "all", 
                                             "Home" = "home", 
                                             "Away" = "away")),
                     DT::dataTableOutput("edit_pitchingTable")
              )
            )
          )
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_gameChanges", "Save Changes", class = "btn-success")
        )
      )
    )
    
    # Render editable batting table
    output$edit_battingTable <- DT::renderDataTable({
      req(current_game_details())
      
      batting_data <- current_game_details()$batting
      game <- current_game_details()$game
      
      # Add calculated columns from global vectors if possible
      player_ids_char <- as.character(batting_data$PlayerID)
      
      # Use global vectors for calculated stats when available
      if (exists("H", envir = .GlobalEnv) && exists("AB", envir = .GlobalEnv)) {
        h_vec <- get("H", envir = .GlobalEnv)
        ab_vec <- get("AB", envir = .GlobalEnv)
        batting_data$H <- as.numeric(h_vec[player_ids_char])
        batting_data$AB <- as.numeric(ab_vec[player_ids_char])
      } else {
        # Calculate locally if needed
        batting_data$H <- batting_data$X1B + batting_data$X2B + 
          batting_data$X3B + batting_data$HR
        batting_data$AB <- batting_data$PA - batting_data$BB - 
          batting_data$HBP - batting_data$SF - batting_data$SH
      }
      
      # Filter by team if selected
      if (!is.null(input$edit_battingTeam) && input$edit_battingTeam != "all") {
        team_name <- if(input$edit_battingTeam == "home") game$HomeName else game$AwayName
        batting_data <- batting_data[batting_data$TeamName == team_name, ]
      }
      
      # Select columns for editing
      display_cols <- c("PlayerName", "TeamName", "PA", "X1B", "X2B", "X3B", "HR", 
                        "BB", "HBP", "SF", "SH", "K", "SB", "CS", "ROE", "RBI")
      
      # Create editable table
      DT::datatable(
        batting_data[, display_cols], 
        rownames = FALSE,
        options = list(
          pageLength = 15, 
          scrollX = TRUE,
          columnDefs = list(
            list(className = 'dt-right', targets = 2:15) # Apply right alignment to numeric columns
          )
        ),
        class = 'cell-border stripe hover',
        editable = list(
          target = "cell",
          disable = list(columns = c(0, 1)) # PlayerName and TeamName are non-editable
        )
      )
    })
    
    # Render editable pitching table
    output$edit_pitchingTable <- DT::renderDataTable({
      req(current_game_details())
      
      pitching_data <- current_game_details()$pitching
      game <- current_game_details()$game
      
      # Filter by team if selected
      if (!is.null(input$edit_pitchingTeam) && input$edit_pitchingTeam != "all") {
        team_name <- if(input$edit_pitchingTeam == "home") game$HomeName else game$AwayName
        pitching_data <- pitching_data[pitching_data$TeamName == team_name, ]
      }
      
      # Select columns for editing
      display_cols <- c("PlayerName", "TeamName", "OutsRecorded", "BF", "H", "R", 
                        "ER", "BB", "K", "HBP", "HR", "GB", "WP")
      
      # Create editable table
      DT::datatable(
        pitching_data[, display_cols], 
        rownames = FALSE,
        options = list(
          pageLength = 15, 
          scrollX = TRUE,
          columnDefs = list(
            list(className = 'dt-right', targets = 2:12)
          )
        ),
        class = 'cell-border stripe hover',
        editable = list(
          target = "cell",
          disable = list(columns = c(0, 1)) # PlayerName and TeamName are non-editable
        )
      )
    })
    
    # Handle edited batting data
    observeEvent(input$edit_battingTable_cell_edit, {
      info <- input$edit_battingTable_cell_edit
      
      # Update the current game details with edited batting data
      game_details <- current_game_details()
      
      # Get column name and convert to appropriate column in database
      # Adding 2 to skip PlayerName and TeamName
      col_idx <- info$col + 1
      col_names <- c("PlayerName", "TeamName", "PA", "X1B", "X2B", "X3B", "HR", 
                     "BB", "HBP", "SF", "SH", "K", "SB", "CS", "ROE", "RBI")
      col_name <- col_names[col_idx]
      
      # Update the value in the stored data
      game_details$batting[info$row + 1, col_name] <- as.integer(info$value)
      
      # Update the reactive value
      current_game_details(game_details)
    })
    
    # Handle edited pitching data
    observeEvent(input$edit_pitchingTable_cell_edit, {
      info <- input$edit_pitchingTable_cell_edit
      
      # Update the current game details with edited pitching data
      game_details <- current_game_details()
      
      # Get column name and convert to appropriate column in database
      # Adding 2 to skip PlayerName and TeamName
      col_idx <- info$col + 1
      col_names <- c("PlayerName", "TeamName", "OutsRecorded", "BF", "H", "R", 
                     "ER", "BB", "K", "HBP", "HR", "GB", "WP")
      col_name <- col_names[col_idx]
      
      # Update the value in the stored data
      game_details$pitching[info$row + 1, col_name] <- as.integer(info$value)
      
      # Update the reactive value
      current_game_details(game_details)
    })
    
    # Handle saving all changes
    observeEvent(input$save_gameChanges, {
      # Get the current game details with any edits
      game_details <- current_game_details()
      game_id <- game_details$game$GameID
      
      # Show a progress indicator
      withProgress(message = "Saving game changes...", value = 0, {
        
        # Flag to track save success
        save_success <- FALSE
        
        tryCatch({
          # Update game record with edited information
          incProgress(0.1, detail = "Updating game details...")
          
          # Get edited values
          home_score <- as.integer(input$edit_homeScore)
          away_score <- as.integer(input$edit_awayScore)
          game_date <- input$edit_gameDate
          
          # Update the game record in the games data frame
          games[games$GameID == game_id, c("HomeScore", "AwayScore", "GameDate")] <- 
            list(home_score, away_score, game_date)
          
          # Update batting statistics
          incProgress(0.3, detail = "Updating batting statistics...")
          
          # Clear existing batting stats for this game
          batting_stats <- batting_stats[batting_stats$GameID != game_id, ]
          
          # Add updated batting stats
          for (i in 1:nrow(game_details$batting)) {
            bat_stat <- game_details$batting[i, ]
            
            # Create a clean record with only the necessary columns
            new_bat <- data.frame(
              GameID = game_id,
              PlayerID = bat_stat$PlayerID,
              PA = as.numeric(bat_stat$PA),
              X1B = as.numeric(bat_stat$X1B),
              X2B = as.numeric(bat_stat$X2B),
              X3B = as.numeric(bat_stat$X3B),
              HR = as.numeric(bat_stat$HR),
              RBI = as.numeric(bat_stat$RBI),
              BB = as.numeric(bat_stat$BB),
              HBP = as.numeric(bat_stat$HBP),
              SF = as.numeric(bat_stat$SF),
              SH = as.numeric(bat_stat$SH),
              K = as.numeric(bat_stat$K),
              SB = as.numeric(bat_stat$SB),
              CS = as.numeric(bat_stat$CS),
              ROE = as.numeric(bat_stat$ROE),
              GIDP = as.numeric(bat_stat$GIDP %||% 0)
            )
            
            # Add to batting_stats
            batting_stats <- rbind(batting_stats, new_bat)
          }
          
          # Update pitching statistics
          incProgress(0.6, detail = "Updating pitching statistics...")
          
          # Clear existing pitching stats for this game
          pitching_stats <- pitching_stats[pitching_stats$GameID != game_id, ]
          
          # Add updated pitching stats
          for (i in 1:nrow(game_details$pitching)) {
            pitch_stat <- game_details$pitching[i, ]
            
            # Create a clean record with only the necessary columns
            new_pitch <- data.frame(
              GameID = game_id,
              PlayerID = pitch_stat$PlayerID,
              BF = as.numeric(pitch_stat$BF),
              H = as.numeric(pitch_stat$H),
              R = as.numeric(pitch_stat$R),
              ER = as.numeric(pitch_stat$ER),
              BB = as.numeric(pitch_stat$BB),
              K = as.numeric(pitch_stat$K),
              HBP = as.numeric(pitch_stat$HBP),
              HR = as.numeric(pitch_stat$HR),
              GB = as.numeric(pitch_stat$GB),
              WP = as.numeric(pitch_stat$WP),
              OutsRecorded = as.numeric(pitch_stat$OutsRecorded)
            )
            
            # Add to pitching_stats
            pitching_stats <- rbind(pitching_stats, new_pitch)
          }
          
          # Save to RDS files
          saveRDS(games, "tblGames.rds")
          saveRDS(batting_stats, "tblBattingStatsFlat.rds")
          saveRDS(pitching_stats, "tblPitchingStatsFlat.rds")
          
          source("R/object_engine.R")
          save_success <- TRUE
          
        }, error = function(e) {
          cat("Error saving game changes:", e$message, "\n")
          showNotification(paste("Error saving game data:", e$message), type = "error")
        })
        
        # If successful, close modal and refresh
        if (save_success) {
          removeModal()
          showNotification("Game data updated successfully!", type = "message")
          
          # Refresh the current view if a game is being viewed
          if (!is.null(input$viewGame) && input$viewGame > 0) {
            # Get updated game details
            updated_details <- tryCatch({
              # Get game details from RDS files
              game <- games[games$GameID == game_id, ]
              
              if (nrow(game) > 0) {
                # Get team names
                home_team <- teams[teams$TeamID == game$HomeTeamID, ]
                away_team <- teams[teams$TeamID == game$AwayTeamID, ]
                
                # Get batting stats for this game
                batting <- batting_stats[batting_stats$GameID == game_id, ]
                if (nrow(batting) > 0) {
                  batting <- merge(batting, players, by = "PlayerID")
                  batting$PlayerName <- paste(batting$FirstName, batting$LastInitial)
                  batting$TeamName <- ifelse(batting$TeamID == game$HomeTeamID, 
                                             home_team$TeamName, away_team$TeamName)
                }
                
                # Get pitching stats for this game
                pitching <- pitching_stats[pitching_stats$GameID == game_id, ]
                if (nrow(pitching) > 0) {
                  pitching <- merge(pitching, players, by = "PlayerID")
                  pitching$PlayerName <- paste(pitching$FirstName, pitching$LastInitial)
                  pitching$TeamName <- ifelse(pitching$TeamID == game$HomeTeamID, 
                                              home_team$TeamName, away_team$TeamName)
                }
                
                # Add team names to game data
                game$HomeName <- home_team$TeamName
                game$AwayName <- away_team$TeamName
                
                list(game = game, batting = batting, pitching = pitching)
              } else {
                NULL
              }
            }, error = function(e) {
              cat("Error refreshing game details:", e$message, "\n")
              NULL
            })
            
            # Update the current game details
            if (!is.null(updated_details) && nrow(updated_details$game) > 0) {
              current_game_details(updated_details)
            }
          }
          
          # Update the completed games dropdown
          update_completed_games()
        }
      })
    })
  })
  
  # --- 14. FILE EXPORT HANDLERS ---
  
  # Handle CSV downloads
  output$downloadBatting <- downloadHandler(
    filename = function() {
      game <- current_game_details()$game
      paste0("batting_stats_", format(as.Date(game$GameDate), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      batting <- current_game_details()$batting
      player_ids_char <- as.character(batting$PlayerID)
      
      # Use global vectors when available
      if (exists("H", envir = .GlobalEnv)) {
        h_vec <- get("H", envir = .GlobalEnv)
        batting$H <- as.numeric(h_vec[player_ids_char])
      } else {
        batting$H <- batting$X1B + batting$X2B + batting$X3B + batting$HR
      }
      
      if (exists("AB", envir = .GlobalEnv)) {
        ab_vec <- get("AB", envir = .GlobalEnv)
        batting$AB <- as.numeric(ab_vec[player_ids_char])
      } else {
        batting$AB <- batting$PA - batting$BB - batting$HBP - batting$SF - batting$SH
      }
      
      # Use global AVG/OBP/SLG vectors if available
      if (exists("AVG", envir = .GlobalEnv)) {
        avg_vec <- get("AVG", envir = .GlobalEnv)
        batting$AVG <- as.numeric(avg_vec[player_ids_char])
      } else {
        batting$AVG <- sapply(1:nrow(batting), function(i) safe_batting_avg(batting$H[i], batting$AB[i]))
      }
      
      if (exists("OBP", envir = .GlobalEnv)) {
        obp_vec <- get("OBP", envir = .GlobalEnv)
        batting$OBP <- as.numeric(obp_vec[player_ids_char])
      } else {
        batting$OBP <- sapply(1:nrow(batting), function(i) safe_obp(batting$H[i], batting$BB[i], batting$HBP[i], batting$AB[i], batting$SF[i]))
      }
      
      if (exists("SLG", envir = .GlobalEnv)) {
        slg_vec <- get("SLG", envir = .GlobalEnv)
        batting$SLG <- as.numeric(slg_vec[player_ids_char])
      } else {
        batting$SLG <- sapply(1:nrow(batting), function(i) {
          total_bases <- batting$X1B[i] + (2 * batting$X2B[i]) + (3 * batting$X3B[i]) + (4 * batting$HR[i])
          safe_slg(total_bases, batting$AB[i])
        })
      }
      
      # Filter by team if selected
      if (!is.null(input$viewBattingTeam) && input$viewBattingTeam != "all") {
        game <- current_game_details()$game
        team_name <- if(input$viewBattingTeam == "home") game$HomeName else game$AwayName
        batting <- batting[batting$TeamName == team_name, ]
      }
      
      # Select columns for export
      export_cols <- c("PlayerName", "TeamName", "AB", "H", "X1B", "X2B", "X3B", "HR", 
                       "RBI", "BB", "K", "AVG", "OBP", "SLG")
      
      write.csv(batting[, export_cols], file, row.names = FALSE)
    }
  )
  
  output$downloadPitching <- downloadHandler(
    filename = function() {
      game <- current_game_details()$game
      paste0("pitching_stats_", format(as.Date(game$GameDate), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      pitching <- current_game_details()$pitching
      player_ids_char <- as.character(pitching$PlayerID)
      
      # Use global vectors when available
      if (exists("IP", envir = .GlobalEnv)) {
        ip_vec <- get("IP", envir = .GlobalEnv)
        pitching$IP <- as.numeric(ip_vec[player_ids_char])
      } else {
        pitching$IP <- pitching$OutsRecorded / 3
      }
      
      # Use global ERA vector if available
      if (exists("ERA", envir = .GlobalEnv)) {
        era_vec <- get("ERA", envir = .GlobalEnv)
        pitching$ERA <- as.numeric(era_vec[player_ids_char])
      } else {
        pitching$ERA <- sapply(1:nrow(pitching), function(i) safe_era(pitching$ER[i], pitching$IP[i]))
      }
      
      # Use global WHIP vector if available 
      if (exists("WHIP", envir = .GlobalEnv)) {
        whip_vec <- get("WHIP", envir = .GlobalEnv)
        pitching$WHIP <- as.numeric(whip_vec[player_ids_char])
      } else {
        pitching$WHIP <- sapply(1:nrow(pitching), function(i) safe_whip(pitching$BB[i], pitching$H[i], pitching$HBP[i], pitching$IP[i]))
      }
      
      # Use global K/9 vector if available
      if (exists("K_per_9", envir = .GlobalEnv)) {
        k9_vec <- get("K_per_9", envir = .GlobalEnv)
        pitching$K_per_9 <- as.numeric(k9_vec[player_ids_char])
      } else {
        pitching$K_per_9 <- sapply(1:nrow(pitching), function(i) safe_per_nine(pitching$K[i], pitching$IP[i]))
      }
      
      # Format IP for display in the traditional baseball format
      pitching$IP_Display <- floor(pitching$OutsRecorded / 3) + (pitching$OutsRecorded %% 3) / 10
      
      # Filter by team if selected
      if (!is.null(input$viewPitchingTeam) && input$viewPitchingTeam != "all") {
        game <- current_game_details()$game
        team_name <- if(input$viewPitchingTeam == "home") game$HomeName else game$AwayName
        pitching <- pitching[pitching$TeamName == team_name, ]
      }
      
      # Select columns for export
      export_cols <- c("PlayerName", "TeamName", "IP_Display", "H", "R", "ER", 
                       "BB", "K", "HR", "ERA", "WHIP", "K_per_9")
      
      write.csv(pitching[, export_cols], file, row.names = FALSE)
    }
  )
  
  # --- 15. INITIALIZATION ON STARTUP ---
  
  # Initialize on startup
  observe({
    # This runs on startup after all data is loaded
    update_completed_games()
    
    # Create master tables for all modules to use
    if (exists("create_master_tables", envir = .GlobalEnv)) {
      tryCatch({
        create_master_tables()
      }, error = function(e) {
        # Silent error handling
      })
    }
  })
  
  # --- 16. MISC UI CLEANUP ---
  
  # Handle view/edit game modal close
  observeEvent(input$close_edit_game, { 
    removeModal() 
  }, ignoreInit = TRUE, once = TRUE)
  
  observeEvent(input$close_view_game, { 
    removeModal() 
  }, ignoreInit = TRUE, once = TRUE)
  
  # Session end handler
  onSessionEnded(function() {
    # Clean up resources if needed
  })
}

# Run the application
shinyApp(ui = ui, server = server)