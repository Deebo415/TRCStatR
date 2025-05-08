# Advanced Metrics and Baseball Career Lab Module
# Combines advanced baseball analytics with career exploration for youth players (ages 11-13)
# Author: Augustine "Gus" Murphy, SABRmetrician

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinyjs)
library(shinycssloaders)
library(shinyBS)

# Source centralized stat and object definitions
source("R/object_engine.R")
source("R/stats.R")

#' Advanced Metrics and Baseball Career Lab UI Module
#' 
#' @param id Module ID
#' @param theme UI theme (optional)
#' @return UI for the combined lab module
baseball_lab_ui <- function(id, theme = NULL) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    fluidRow(
      column(12,
             h2("Advanced Metrics and Baseball Career Lab", class = "lab-module-title"),
             p(class = "lab-module-intro", "Explore the science behind baseball statistics and discover exciting careers in America's favorite pastime!")
      )
    ),
    
    tabsetPanel(
      id = ns("lab_tabs"),
      
      # Tab 1: Advanced Metrics Explorer
      tabPanel(
        "Advanced Metrics Explorer",
        fluidRow(
          column(12,
                 # Achievements banner at top
                 achievements_ui(ns("achievements"))
          )
        ),
        fluidRow(
          column(12,
                 h3("How Baseball Statistics Have Evolved", class = "lab-section-header"),
                 baseball_stats_timeline_ui(ns("stats_timeline")),
                 hr()
          )
        ),
        fluidRow(
          column(12,
                 h3("Compare Your Stats", class = "lab-section-header"),
                 p("Select metrics and players to create your own statistical comparison"),
                 # Advanced metrics wizard UI
                 advanced_metrics_wizard_ui(ns("metrics_wizard"))
          )
        ),
        fluidRow(
          column(12,
                 h3("How The Pros Use It", class = "lab-section-header"),
                 pro_stats_usage_ui(ns("pro_usage"))
          )
        )
      ),
      
      # Tab 2: Baseball Careers Explorer
      tabPanel(
        "Baseball Careers Explorer",
        fluidRow(
          column(12,
                 div(class = "careers-intro-panel",
                     h3("Your Baseball Journey Doesn't End Here", class = "lab-section-header"),
                     p("Did you know? There are thousands of exciting careers in baseball beyond being a professional player. Find careers that match your interests and strengths!"),
                     tags$ul(
                       tags$li(strong("Only about 1 in 200 high school baseball players"), "will be drafted by an MLB team"),
                       tags$li("But there are", strong("over 150,000 people"), "employed in baseball-related jobs across America"),
                       tags$li("Your love for baseball can become a career in many different ways")
                     )
                 )
          )
        ),
        fluidRow(
          column(4,
                 div(class = "explore-options-panel",
                     h4("How would you like to explore?"),
                     radioButtons(ns("exploration_method"),
                                  label = NULL,
                                  choices = c(
                                    "See all careers" = "all",
                                    "Filter by what I'm good at" = "subjects",
                                    "Take a career match quiz" = "quiz"
                                  ))
                 )
          ),
          column(8,
                 uiOutput(ns("career_exploration_content"))
          )
        )
      )
    )
  )
}

#' Advanced Metrics and Baseball Career Lab Server Module
#' 
#' @param id Module ID
#' @param players_df Data frame of players
#' @param batting_stats_df Data frame of batting stats
#' @param pitching_stats_df Data frame of pitching stats
#' @param league_averages List or data frame of league averages
#' @return Server logic for the combined lab module
baseball_lab_server <- function(id, players_df, batting_stats_df, pitching_stats_df, league_averages) {
  moduleServer(id, function(input, output, session) {
    
    # Load career data (could be moved to a separate file later)
    careers_data <- get_baseball_careers_data()
    
    # Initialize the achievements system
    achievements_server("achievements", reactive({ get_current_player_id() }))
    
    # Initialize the baseball stats timeline component
    baseball_stats_timeline_server("stats_timeline")
    
    # Initialize the metrics visualization component
    advanced_metrics_wizard_server("metrics_wizard", players_df, batting_stats_df, pitching_stats_df, league_averages)
    
    # Initialize the pro usage component
    pro_stats_usage_server("pro_usage")
    
    # Handle career exploration content based on selected method
    output$career_exploration_content <- renderUI({
      req(input$exploration_method)
      
      switch(input$exploration_method,
             "all" = baseball_careers_browse_ui(session$ns("careers_browse")),
             "subjects" = baseball_careers_by_subject_ui(session$ns("careers_by_subject")),
             "quiz" = baseball_career_quiz_ui(session$ns("career_quiz"))
      )
    })
    
    # Initialize career exploration components when selected
    observeEvent(input$exploration_method, {
      if(input$exploration_method == "all") {
        baseball_careers_browse_server("careers_browse", careers_data)
      } else if(input$exploration_method == "subjects") {
        baseball_careers_by_subject_server("careers_by_subject", careers_data)
      } else if(input$exploration_method == "quiz") {
        baseball_career_quiz_server("career_quiz", careers_data)
      }
    })
    
    # Helper function to get current player ID
    get_current_player_id <- reactive({
      # In a real app, this would get the logged-in player's ID
      # For now, return a default or the first selected player
      if (!is.null(input$selected_player)) {
        return(input$selected_player)
      } else {
        return("default_player")
      }
    })
  })
}

# ===== ACHIEVEMENTS SYSTEM =====

#' Achievements UI Component
#' 
#' @param id Module ID
#' @return UI for the achievements system
achievements_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "achievement-banner",
    icon("trophy"), 
    span("Baseball Stats Achievements"),
    actionButton(ns("view_all_achievements"), "View All", 
                 class = "btn-sm btn-info")
  )
}

#' Achievements Server Component
#' 
#' @param id Module ID
#' @param player_id Reactive expression returning the current player's ID
#' @return Server logic for the achievements system
achievements_server <- function(id, player_id) {
  moduleServer(id, function(input, output, session) {
    
    # Achievement definitions
    baseball_achievements <- list(
      list(
        id = "metrics_explorer",
        name = "Metrics Explorer",
        description = "View details about 5 different advanced metrics",
        badge_icon = "medal",
        difficulty = "easy"
      ),
      list(
        id = "stats_guru",
        name = "Stats Guru",
        description = "Correctly answer 10 questions about advanced metrics",
        badge_icon = "brain",
        difficulty = "medium"
      ),
      list(
        id = "team_comparison",
        name = "Team Analyst",
        description = "Compare stats with 3 teammates",
        badge_icon = "users", 
        difficulty = "easy"
      ),
      list(
        id = "league_comparison",
        name = "League Analyst",
        description = "Compare stats with players from 3 different teams",
        badge_icon = "chart-line",
        difficulty = "medium"
      ),
      list(
        id = "career_explorer",
        name = "Career Explorer",
        description = "Discover 10 different baseball careers",
        badge_icon = "briefcase",
        difficulty = "easy"
      ),
      list(
        id = "vocational_explorer",
        name = "Hands-On Explorer",
        description = "Learn about 5 different vocational careers in baseball",
        badge_icon = "tools",
        difficulty = "easy"
      )
    )
    
    # Modal to show all achievements
    observeEvent(input$view_all_achievements, {
      # This would be implemented to show current progress
      showModal(modalDialog(
        title = "Your Baseball Stats Achievements",
        "Achievement system coming soon! Check back later to track your progress.",
        easyClose = TRUE
      ))
    })
  })
}

# ===== BASEBALL STATS TIMELINE =====

#' Baseball Stats Timeline UI Component
#' 
#' @param id Module ID
#' @return UI for the baseball stats timeline
baseball_stats_timeline_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "stats-timeline-container",
    p("Baseball statistics have evolved dramatically over time, from basic counting stats to today's advanced metrics. Explore this timeline to see how our understanding of the game has changed."),
    div(
      class = "timeline-controls",
      actionButton(ns("timeline_era_traditional"), "Traditional Era", class = "btn-era"),
      actionButton(ns("timeline_era_sabermetric"), "Sabermetric Revolution", class = "btn-era"),
      actionButton(ns("timeline_era_statcast"), "Statcast Era", class = "btn-era")
    ),
    div(
      id = ns("timeline_display"),
      class = "timeline-display"
    )
  )
}

#' Baseball Stats Timeline Server Component
#' 
#' @param id Module ID
#' @return Server logic for the baseball stats timeline
baseball_stats_timeline_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Timeline data
    timeline_data <- list(
      traditional = list(
        era_name = "Traditional Stats Era (1876-1970s)",
        description = "Baseball's earliest statistics focused on counting basic outcomes that were easy to track during games.",
        key_stats = list(
          list(
            year = "1876",
            stat = "Batting Average (AVG)",
            description = "The original batting statistic, showing hits per at-bat"
          ),
          list(
            year = "1920",
            stat = "Earned Run Average (ERA)",
            description = "Measures a pitcher's effectiveness by runs allowed per 9 innings"
          ),
          list(
            year = "1954",
            stat = "Runs Batted In (RBI)",
            description = "Became a primary measure of a hitter's contribution to scoring"
          )
        )
      ),
      
      sabermetric = list(
        era_name = "Sabermetric Revolution (1980s-2010)",
        description = "Advanced statistical analysis that looked beyond traditional counting stats to better evaluate player performance.",
        key_stats = list(
          list(
            year = "1984",
            stat = "On-base Plus Slugging (OPS)",
            description = "Combined on-base percentage and slugging percentage for a more complete picture of offensive value"
          ),
          list(
            year = "2002",
            stat = "Wins Above Replacement (WAR)",
            description = "Popularized by Moneyball, attempts to calculate a player's total contribution to their team"
          ),
          list(
            year = "2007",
            stat = "Fielding Independent Pitching (FIP)",
            description = "Measures pitcher performance independent of fielding quality behind them"
          )
        )
      ),
      
      statcast = list(
        era_name = "Statcast Era (2015-Present)",
        description = "Uses advanced tracking technology to measure player movements and ball physics with incredible precision.",
        key_stats = list(
          list(
            year = "2015",
            stat = "Exit Velocity",
            description = "Measures how fast the ball comes off the bat, indicating quality of contact"
          ),
          list(
            year = "2016",
            stat = "Launch Angle",
            description = "The vertical angle of the ball leaving the bat, revolutionizing swing approaches"
          ),
          list(
            year = "2020",
            stat = "Outs Above Average (OAA)",
            description = "Defensive metric using precise player positioning data to evaluate fielding skill"
          )
        )
      )
    )
    
    # Default to showing traditional era on load
    current_era <- reactiveVal("traditional")
    
    # Update display when buttons are clicked
    observeEvent(input$timeline_era_traditional, {
      current_era("traditional")
    })
    
    observeEvent(input$timeline_era_sabermetric, {
      current_era("sabermetric")
    })
    
    observeEvent(input$timeline_era_statcast, {
      current_era("statcast")
    })
    
    # Render the timeline display
    observe({
      era <- current_era()
      era_data <- timeline_data[[era]]
      
      output_html <- div(
        class = paste0("timeline-era timeline-", era),
        h3(era_data$era_name, class = "era-title"),
        p(era_data$description, class = "era-desc"),
        div(
          class = "timeline-stats",
          lapply(era_data$key_stats, function(stat) {
            div(
              class = "timeline-stat-card",
              div(class = "stat-year", stat$year),
              div(
                class = "stat-details",
                h4(stat$stat),
                p(stat$description),
                if(era == "traditional") {
                  p(class = "stat-connection", "Still Used Today: This stat is still a fundamental part of baseball.")
                } else if(era == "sabermetric") {
                  p(class = "stat-connection", "Advanced View: This stat provides deeper insight than traditional stats.")
                } else {
                  p(class = "stat-connection", "Technology Driven: This stat wouldn't be possible without modern tracking systems.")
                }
              )
            )
          })
        )
      )
      
      # Insert HTML to container
      session$output[[session$ns("timeline_display")]] <- renderUI({ output_html })
    })
  })
}

# ===== ADVANCED METRICS WIZARD =====

#' Advanced Metrics Wizard UI Component
#' Based on the original advanced_metrics_module.R but integrated into the new structure
#' 
#' @param id Module ID
#' @return UI for the advanced metrics wizard
advanced_metrics_wizard_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("step_ui"))
  )
}

#' Advanced Metrics Wizard Server Component
#' Based on the original advanced_metrics_module.R but integrated into the new structure
#' 
#' @param id Module ID
#' @param players_df Data frame of players
#' @param batting_stats_df Data frame of batting stats
#' @param pitching_stats_df Data frame of pitching stats
#' @param league_averages List or data frame of league averages
#' @return Server logic for the advanced metrics wizard
advanced_metrics_wizard_server <- function(id, players_df, batting_stats_df, pitching_stats_df, league_averages) {
  moduleServer(id, function(input, output, session) {
    # This would contain the core of your existing advanced_metrics_server function
    current_step <- reactiveVal(1)
    selected_metrics <- reactiveValues(val = character(0))
    
    # --- Reactive to fetch player data from the database ---
    players_data <- reactive({
      req(players_df)
      if (!is.null(players_df) && nrow(players_df) > 0) {
        if ("TeamName" %in% names(players_df)) {
          players_df[order(players_df$TeamName, players_df$FirstName, players_df$LastInitial), ]
        } else {
          players_df[order(players_df$FirstName, players_df$LastInitial), ]
        }
      } else {
        NULL
      }
    })
    
    output$step_ui <- renderUI({
      step <- current_step()
      ns <- session$ns
      if (step == 1) {
        tagList(
          div('STEP 1: Select Stat Type'),
          radioButtons(
            ns("stat_type"),
            label = "Choose Stat Type:",
            choices = c("Batting" = "batting", "Pitching" = "pitching"),
            selected = "batting",
            inline = TRUE
          ),
          actionButton(ns("next_step1"), "Next")
        )
      } else if (step == 2) {
        # Define advanced metrics for each stat type
        batting_metrics <- c("OPS", "OPS+", "wOBA", "BABIP", "ISO", "wRC+", "bWAR")
        pitching_metrics <- c("pWAR", "ERA", "ERA+", "WHIP", "WHIP+", "FIP", "K/9", "BB/9", "K/BB")
        stat_type <- input$stat_type
        metrics_choices <- if (!is.null(stat_type) && stat_type == "pitching") pitching_metrics else batting_metrics
        tagList(
          div('STEP 2: Select Advanced Metrics'),
          fluidRow(
            column(6, actionButton(ns("select_all_metrics"), "Select All", class = "btn-primary")),
            column(6, actionButton(ns("clear_all_metrics"), "Clear All", class = "btn-secondary"))
          ),
          uiOutput(ns("metrics_grid")),
          tags$style(HTML('
        .metric-card { border: 2px solid #ccc; border-radius: 8px; padding: 12px; margin-bottom: 12px; background: #f9f9f9; cursor: pointer; min-height: 110px; transition: border-color 0.2s; }
        .metric-card.selected { border-color: #007bff; background: #eaf3ff; box-shadow: 0 2px 8px rgba(0,0,0,0.04); }
        .metric-title { font-size: 1.3em; font-weight: bold; margin-bottom: 4px; }
        .metric-learn { font-size: 90%; margin-left: 5px; }
      ')),
          fluidRow(
            column(6, actionButton(ns("back_step2"), "Back", class = "btn-secondary")),
            column(6, actionButton(ns("next_step2"), "Next", class = "btn-success"))
          )
        )
      } else if (step == 3) {
        tagList(
          div('STEP 3: Select Players'),
          {
            players <- players_data()
            if (is.null(players) || nrow(players) == 0) {
              tagList(
                div(style = "color: #b00; font-weight: bold; margin: 20px 0;", "No players available for selection. Please check your database or add players to continue."),
                fluidRow(
                  column(6, actionButton(ns("back_step3"), "Back", class = "btn-secondary")),
                  column(6, actionButton(ns("next_step3"), "Next", class = "btn-success", disabled = TRUE))
                )
              )
            } else {
              display_names <- paste0(players$FirstName, " ", players$LastInitial, " (", players$TeamName, ")")
              selectizeInput(
                ns("selected_players"),
                label = "Choose Players:",
                choices = setNames(as.character(players$PlayerID), display_names),
                multiple = TRUE
              )
            }
          },
          fluidRow(
            column(6, actionButton(ns("back_step3"), "Back", class = "btn-secondary")),
            column(6, actionButton(ns("next_step3"), "Next", class = "btn-success"))
          )
        )
      } else if (step == 4) {
        tagList(
          div('STEP 4: Metrics Results / Visualization'),
          tags$h4("Summary of Selections:"),
          tags$b("Stat Type: "), input$stat_type, tags$br(),
          tags$b("Metrics: "), paste(selected_metrics$val, collapse = ", "), tags$br(),
          tags$b("Players: "), paste(input$selected_players, collapse = ", ") , tags$hr(),
          # --- Explanations for selected metrics ---
          uiOutput(ns("metric_explanations")),
          # --- Real metrics table ---
          DT::dataTableOutput(ns("metrics_table")),
          tags$hr(),
          fluidRow(
            column(6, plotly::plotlyOutput(ns("metrics_radar"))),
            column(6, plotly::plotlyOutput(ns("metrics_barchart")))
          ),
          tags$hr(),
          fluidRow(
            column(6, actionButton(ns("back_step4"), "Back", class = "btn-secondary")),
            column(6, actionButton(ns("start_over"), "Start Over", class = "btn-danger"))
          )
        )
      }
    })
    
    # Include the rest of your existing advanced_metrics_server code here
    # (metrics_table_data reactive, metric_explanations output, metrics_table output, etc.)
    # I'm omitting the full implementation for brevity, but it would be copied from
    # your existing advanced_metrics_module.R file
    
    # Navigation buttons
    observeEvent(input$next_step1, {
      current_step(2)
    })
    
    observeEvent(input$back_step2, {
      current_step(1)
    })
    
    observeEvent(input$next_step2, {
      current_step(3)
    })
    
    observeEvent(input$back_step3, {
      current_step(2)
    })
    
    observeEvent(input$next_step3, {
      current_step(4)
    })
    
    observeEvent(input$back_step4, {
      current_step(3)
    })
    
    observeEvent(input$start_over, {
      current_step(1)
      selected_metrics$val <- character(0)
    })
  })
}

# ===== PRO STATS USAGE =====

#' Pro Stats Usage UI Component
#' 
#' @param id Module ID
#' @return UI for the pro stats usage examples
pro_stats_usage_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "pro-stats-usage-container",
    p("Major League teams use advanced metrics to make important decisions. Here's how MLB teams put these numbers to work:"),
    div(
      class = "pro-usage-tabs",
      tabsetPanel(
        id = ns("usage_tabs"),
        tabPanel(
          "Player Evaluation",
          div(
            class = "usage-content",
            div(
              class = "usage-example",
              h4("Front Office Example"),
              p("The Tampa Bay Rays use advanced metrics to find undervalued players, allowing them to compete with teams that have much bigger budgets."),
              tags$blockquote(
                class = "usage-quote",
                "We have to be very creative in how we try to create advantages. We are never going to be able to use dollars to create advantages - we use information.",
                tags$footer("Andrew Friedman, former Rays GM")
              )
            ),
            div(
              class = "usage-stats",
              h4("Key Stats Used:"),
              tags$ul(
                tags$li(tags$strong("WAR (Wins Above Replacement)"), " - Helps teams compare players across positions"),
                tags$li(tags$strong("wRC+ (Weighted Runs Created Plus)"), " - Shows a player's overall offensive value"),
                tags$li(tags$strong("BABIP (Batting Average on Balls In Play)"), " - Identifies players who might be experiencing good/bad luck")
              )
            )
          )
        ),
        tabPanel(
          "Game Strategy",
          div(
            class = "usage-content",
            div(
              class = "usage-example",
              h4("Defensive Shifts Example"),
              p("Teams now position their defenders based on where each batter is most likely to hit the ball, using detailed spray charts of every player."),
              tags$blockquote(
                class = "usage-quote",
                "When we put a shift on, it's based on what the data's telling us. We're trying to put our players in the best position to get outs.",
                tags$footer("MLB Manager")
              )
            ),
            div(
              class = "usage-stats",
              h4("Key Stats Used:"),
              tags$ul(
                tags$li(tags$strong("Spray Charts"), " - Show where each batter typically hits the ball"),
                tags$li(tags$strong("Launch Angle"), " - Helps predict whether a batter hits more grounders or fly balls"),
                tags$li(tags$strong("Pitch-by-pitch Tendencies"), " - How batters respond to different pitch types and locations")
              )
            )
          )
        ),
        tabPanel(
          "Player Development",
          div(
            class = "usage-content",
            div(
              class = "usage-example",
              h4("Pitching Development Example"),
              p("The Houston Astros have become famous for using advanced data to help pitchers improve their performance by adjusting their pitch selection and mechanics."),
              tags$blockquote(
                class = "usage-quote",
                "We showed him the data about his curveball being elite, and suddenly he started throwing it 40% of the time instead of 10%, and his results completely changed.",
                tags$footer("MLB Pitching Coach")
              )
            ),
            div(
              class = "usage-stats",
              h4("Key Stats Used:"),
              tags$ul(
                tags$li(tags$strong("Spin Rate"), " - How fast a pitch spins, affecting its movement"),
                tags$li(tags$strong("Pitch Tunneling"), " - How well pitches look the same out of the hand before breaking differently"),
                tags$li(tags$strong("Expected Outcomes"), " - What would typically happen with each pitch type based on quality of contact")
              )
            )
          )
        )
      )
    )
  )
}

#' Pro Stats Usage Server Component
#' 
#' @param id Module ID
#' @return Server logic for the pro stats usage examples
pro_stats_usage_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Nothing to do here yet - could add interactivity later
  })
}

# ===== BASEBALL CAREERS BROWSE =====

#' Baseball Careers Browse UI Component
#' 
#' @param id Module ID
#' @return UI for browsing all baseball careers
baseball_careers_browse_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "careers-browse-container",
    h3("Explore All Baseball Careers"),
    p("Baseball offers an incredible variety of careers! Browse through these categories to discover jobs that might interest you:"),
    
    # Categories tabset
    tabsetPanel(
      id = ns("career_categories"),
      tabPanel(
        "Data & Analytics",
        uiOutput(ns("data_analytics_careers"))
      ),
      tabPanel(
        "Stadium & Facilities",
        uiOutput(ns("stadium_facilities_careers")) 
      ),
      tabPanel(
        "Legal & Business",
        uiOutput(ns("legal_business_careers"))
      ),
      tabPanel(
        "Team Operations",
        uiOutput(ns("team_operations_careers"))
      ),
      tabPanel(
        "Technology & Media",
        uiOutput(ns("technology_media_careers"))
      ),
      tabPanel(
        "Science & Medicine",
        uiOutput(ns("science_medicine_careers"))
      ),
      tabPanel(
        "Game Day Operations",
        uiOutput(ns("game_day_careers"))
      ),
      tabPanel(
        "Equipment & Gear",
        uiOutput(ns("equipment_gear_careers"))
      )
    )
  )
}

#' Baseball Careers Browse Server Component
#' 
#' @param id Module ID
#' @param careers_data List of baseball career data
#' @return Server logic for browsing all baseball careers
baseball_careers_browse_server <- function(id, careers_data) {
  moduleServer(id, function(input, output, session) {
    
    # Render each category's careers
    output$data_analytics_careers <- renderUI({
      category_data <- careers_data[["Data & Analytics"]]
      render_category_careers(category_data, session$ns)
    })
    
    output$stadium_facilities_careers <- renderUI({
      category_data <- careers_data[["Stadium & Facilities"]]
      render_category_careers(category_data, session$ns)
    })
    
    output$legal_business_careers <- renderUI({
      category_data <- careers_data[["Legal & Business"]]
      render_category_careers(category_data, session$ns)
    })
    
    output$team_operations_careers <- renderUI({
      category_data <- careers_data[["Team Operations"]]
      render_category_careers(category_data, session$ns)
    })
    
    output$technology_media_careers <- renderUI({
      category_data <- careers_data[["Technology & Media"]]
      render_category_careers(category_data, session$ns)
    })
    
    output$science_medicine_careers <- renderUI({
      category_data <- careers_data[["Science & Medicine"]]
      render_category_careers(category_data, session$ns)
    })
    
    output$game_day_careers <- renderUI({
      category_data <- careers_data[["Game Day Operations"]]
      render_category_careers(category_data, session$ns)
    })
    
    output$equipment_gear_careers <- renderUI({
      category_data <- careers_data[["Equipment & Gear"]]
      render_category_careers(category_data, session$ns)
    })
    
    # Helper function to render careers in a category
    render_category_careers <- function(category_data, ns) {
      tagList(
        p(class = "category-description", "Click on any career to learn more about educational requirements, skills needed, and how to get started."),
        div(
          class = "career-cards",
          lapply(names(category_data), function(career_name) {
            career <- category_data[[career_name]]
            
            div(
              class = "career-card",
              div(
                class = "career-header",
                h5(career_name)
              ),
              div(
                class = "career-body",
                p(career$description),
                div(
                  class = "career-subjects",
                  tags$strong("School Subjects:"),
                  tags$ul(
                    lapply(career$subjects, function(subject) {
                      tags$li(subject)
                    })
                  )
                )
              ),
              div(
                class = "career-footer",
                actionButton(
                  ns(paste0("view_career_", gsub(" ", "_", career_name))),
                  "Learn More",
                  class = "btn-sm btn-info",
                  onclick = paste0("Shiny.setInputValue('", ns("view_career"), "', '", gsub(" ", "_", career_name), "', {priority: 'event'});")
                )
              )
            )
          })
        )
      )
    }
    
    # Handle career detail views
    observeEvent(input$view_career, {
      career_id <- input$view_career
      career_name <- gsub("_", " ", career_id)
      
      # Find the career in the data
      career_data <- NULL
      category_name <- NULL
      
      for (cat_name in names(careers_data)) {
        if (career_name %in% names(careers_data[[cat_name]])) {
          career_data <- careers_data[[cat_name]][[career_name]]
          category_name <- cat_name
          break
        }
      }
      
      if (!is.null(career_data)) {
        # Show career detail modal
        showModal(modalDialog(
          title = career_name,
          size = "large",
          
          div(
            class = "career-detail-content",
            div(
              class = "career-detail-header",
              span(class = "career-category-label", category_name)
            ),
            div(
              class = "career-detail-body",
              div(
                class = "career-section",
                h4("What You'll Do"),
                p(career_data$description),
                p(career_data$daily_tasks)
              ),
              div(
                class = "career-section",
                h4("Education Needed"),
                p(career_data$education),
                h5("School Subjects to Focus On Now"),
                div(
                  class = "subject-focus-list",
                  lapply(career_data$subjects, function(subject) {
                    div(
                      class = "subject-focus-item",
                      h6(subject),
                      p(career_data$subject_relevance[[subject]])
                    )
                  })
                )
              ),
              div(
                class = "career-section",
                h4("Skills You'll Use"),
                p(career_data$skills)
              ),
              div(
                class = "career-section",
                h4("How to Get Started Now"),
                p("Even at your age, there are ways to begin building skills for this career:"),
                tags$ul(
                  lapply(career_data$start_now, function(tip) {
                    tags$li(tip)
                  })
                )
              )
            ),
            div(
              class = "career-detail-footer",
              actionButton(session$ns("close_career_detail"), "Close", class = "btn-secondary")
            )
          ),
          
          footer = NULL,
          easyClose = TRUE
        ))
      }
    })
    
    # Handle close button in career detail modal
    observeEvent(input$close_career_detail, {
      removeModal()
    })
  })
}

# ===== BASEBALL CAREERS BY SUBJECT =====

#' Baseball Careers by Subject UI Component
#' 
#' @param id Module ID
#' @return UI for finding careers based on school subjects
baseball_careers_by_subject_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "careers-by-subject-container",
    div(
      class = "subject-selection",
      h3("Find Baseball Careers Based on School Subjects You Like"),
      p("Select subjects you enjoy or excel at to discover baseball careers that use those skills:"),
      checkboxGroupInput(
        ns("selected_subjects"),
        label = NULL,
        choices = c(
          "Math" = "math",
          "Science (Biology/Chemistry/Physics)" = "science",
          "Computer Science/Technology" = "tech",
          "English/Writing/Communication" = "communication",
          "Art/Design" = "design",
          "Physical Education/Sports" = "sports",
          "Social Studies/History" = "social",
          "Technical Education/Shop Class" = "technical"
        ),
        selected = c("math")
      ),
      actionButton(ns("find_careers"), "Find My Baseball Careers", class = "btn-primary")
    ),
    uiOutput(ns("matching_careers"))
  )
}

#' Baseball Careers by Subject Server Component
#' 
#' @param id Module ID
#' @param careers_data List of baseball career data
#' @return Server logic for finding careers based on school subjects
baseball_careers_by_subject_server <- function(id, careers_data) {
  moduleServer(id, function(input, output, session) {
    
    # Subject mapping 
    subject_mapping <- list(
      "math" = c("Math", "Statistics", "Economics"),
      "science" = c("Biology", "Chemistry", "Physics"),
      "tech" = c("Computer Science", "Information Technology", "Electronics"),
      "communication" = c("Writing", "Communication", "Journalism", "Public Speaking"),
      "design" = c("Art", "Design", "Architecture"),
      "sports" = c("Physical Education", "Health", "Kinesiology"),
      "social" = c("Government", "Psychology", "History", "Geography"),
      "technical" = c("Technical Education", "Engineering", "Materials Science")
    )
    
    # Find careers matching selected subjects
    matching_careers <- eventReactive(input$find_careers, {
      req(input$selected_subjects)
      
      # Expand selected subjects to actual subject names
      expanded_subjects <- unlist(subject_mapping[input$selected_subjects])
      
      # Filter careers that match any of the expanded subjects
      matches <- list()
      
      for (category_name in names(careers_data)) {
        category <- careers_data[[category_name]]
        
        for (career_name in names(category)) {
          career <- category[[career_name]]
          
          # Check if any of the career's subjects match the expanded subjects
          if (any(career$subjects %in% expanded_subjects)) {
            # Add to matches with category info
            matches[[career_name]] <- c(career, list(
              name = career_name, 
              category = category_name
            ))
          }
        }
      }
      
      return(matches)
    })
    
    # Display matching careers
    output$matching_careers <- renderUI({
      careers <- matching_careers()
      
      if (length(careers) == 0) {
        return(div(
          class = "no-matches",
          h4("No exact matches found."),
          p("Try selecting different or additional subjects, or check out the complete list of careers.")
        ))
      }
      
      # Sort careers by category for display
      careers_by_category <- list()
      
      for (career_name in names(careers)) {
        career <- careers[[career_name]]
        category <- career$category
        
        if (!(category %in% names(careers_by_category))) {
          careers_by_category[[category]] <- list()
        }
        
        careers_by_category[[category]][[career_name]] <- career
      }
      
      # Create the output with category headers
      output_html <- div(
        class = "matching-careers-results",
        h3(paste0("We found ", length(careers), " baseball careers that match your interests!")),
        
        lapply(names(careers_by_category), function(category_name) {
          div(
            class = "career-category",
            h4(category_name),
            div(
              class = "career-cards",
              lapply(names(careers_by_category[[category_name]]), function(career_name) {
                career <- careers_by_category[[category_name]][[career_name]]
                
                # Match the subjects with the selected subjects
                highlighted_subjects <- intersect(
                  career$subjects, 
                  unlist(subject_mapping[input$selected_subjects])
                )
                
                div(
                  class = "career-card",
                  div(
                    class = "career-header",
                    h5(career_name),
                    span(class = "career-category-label", category_name)
                  ),
                  div(
                    class = "career-body",
                    p(career$description),
                    div(
                      class = "subject-matches",
                      tags$strong("Matching Subjects:"),
                      tags$ul(
                        lapply(highlighted_subjects, function(subject) {
                          tags$li(subject, class = "matched-subject")
                        })
                      )
                    )
                  ),
                  div(
                    class = "career-footer",
                    actionButton(
                      session$ns(paste0("view_career_", gsub(" ", "_", career_name))),
                      "Learn More",
                      class = "btn-sm btn-info",
                      onclick = paste0("Shiny.setInputValue('", session$ns("view_career"), "', '", gsub(" ", "_", career_name), "', {priority: 'event'});")
                    )
                  )
                )
              })
            )
          )
        }),
        
        div(
          class = "careers-suggestion",
          hr(),
          p(tags$em("Don't see a career that interests you? Try selecting different subjects or check out the full list of baseball careers.")),
          actionButton(ns("view_all_careers"), "View All Baseball Careers", class = "btn-secondary")
        )
      )
      
      return(output_html)
    })
    
    # Handle career detail views (similar to browse component)
    observeEvent(input$view_career, {
      career_id <- input$view_career
      career_name <- gsub("_", " ", career_id)
      
      # Find the career data and show modal
      # (Same code as in the browse component)
    })
    
    # Handle view all careers button
    observeEvent(input$view_all_careers, {
      # Switch to "all careers" view
      updateRadioButtons(session$parent, "exploration_method", selected = "all")
    })
  })
}

# ===== BASEBALL CAREER QUIZ =====

#' Baseball Career Quiz UI Component
#' 
#' @param id Module ID
#' @return UI for the career match quiz
baseball_career_quiz_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "career-quiz-container",
      h3("Find Your Baseball Career Match"),
      p("Answer these questions to discover baseball careers that might be perfect for you!"),
      
      div(
        class = "quiz-progress",
        div(class = "progress", 
            div(class = "progress-bar", id = ns("quiz_progress"), 
                role = "progressbar", style = "width: 0%;",
                "aria-valuenow" = "0", "aria-valuemin" = "0", "aria-valuemax" = "100")
        )
      ),
      
      # Question container
      div(id = ns("question_container"), class = "question-container",
          # Questions will be inserted here dynamically
      ),
      
      # Navigation buttons
      div(class = "quiz-navigation",
          actionButton(ns("prev_question"), "Previous", class = "btn-secondary"),
          actionButton(ns("next_question"), "Next", class = "btn-primary")
      ),
      
      # Results (initially hidden)
      div(id = ns("quiz_results"), class = "quiz-results",
          style = "display: none;",
          h3("Your Top Career Matches"),
          uiOutput(ns("career_matches"))
      )
    )
  )
}

#' Baseball Career Quiz Server Component
#' 
#' @param id Module ID
#' @param careers_data List of baseball career data
#' @return Server logic for the career match quiz
baseball_career_quiz_server <- function(id, careers_data) {
  moduleServer(id, function(input, output, session) {
    
    # Questions for the quiz
    questions <- list(
      list(
        id = "subject_preference",
        text = "Which school subjects do you enjoy the most?",
        type = "multiple",
        options = c("Math", "Science", "English/Writing", "Computer/Technology", 
                    "Art/Design", "Social Studies/History", "Physical Education",
                    "Technical Education/Shop Class"),
        max_selections = 3
      ),
      list(
        id = "working_style",
        text = "How do you prefer to work?",
        type = "single",
        options = c("Alone focusing deeply on tasks", 
                    "In a small team collaborating", 
                    "Moving around and being active",
                    "Helping and teaching others")
      ),
      list(
        id = "baseball_interest",
        text = "What aspects of baseball interest you the most?",
        type = "multiple",
        options = c("Statistics and numbers", 
                    "The business side", 
                    "Equipment and technology",
                    "Team building and management",
                    "Health and physical performance",
                    "Media and communication",
                    "Stadium operations"),
        max_selections = 3
      ),
      list(
        id = "career_values",
        text = "What's most important to you in a future career?",
        type = "single",
        options = c("Making a good salary", 
                    "Having job security", 
                    "Doing something exciting",
                    "Helping others",
                    "Being creative",
                    "Solving interesting problems")
      ),
      list(
        id = "technical_preference",
        text = "Do you enjoy building or fixing things with your hands?",
        type = "single",
        options = c("Yes, I love hands-on work", 
                    "Sometimes, it depends on the project", 
                    "No, I prefer mental challenges",
                    "I've never really tried it")
      )
    )
    
    # Current question index
    current_question <- reactiveVal(1)
    
    # Store answers
    answers <- reactiveValues()
    
    # Initialize answers
    observe({
      for(q in questions) {
        answers[[q$id]] <- NULL
      }
    }, priority = 1000)
    
    # Render current question
    observe({
      q_idx <- current_question()
      
      # Remove previous questions
      removeUI(selector = paste0("#", session$ns("question_container"), " > *"))
      
      # Check if we're done with questions
      if(q_idx > length(questions)) {
        # Hide question container and show results
        shinyjs::hide("question_container")
        shinyjs::hide("quiz_navigation")
        shinyjs::show("quiz_results")
        return()
      }
      
      q <- questions[[q_idx]]
      
      # Create UI for current question
      output_html <- switch(q$type,
                            "single" = radioButtons(
                              session$ns(paste0("q_", q$id)),
                              q$text,
                              choices = q$options,
                              selected = answers[[q$id]]
                            ),
                            "multiple" = checkboxGroupInput(
                              session$ns(paste0("q_", q$id)),
                              q$text,
                              choices = q$options,
                              selected = answers[[q$id]]
                            )
      )
      
      # Insert HTML to container
      insertUI(
        selector = paste0("#", session$ns("question_container")),
        where = "beforeEnd",
        ui = output_html,
        immediate = TRUE
      )
      
      # Update progress bar
      progress_pct <- (q_idx / length(questions)) * 100
      shinyjs::runjs(paste0('document.getElementById("', session$ns("quiz_progress"), '").style.width = "', progress_pct, '%";'))
      shinyjs::runjs(paste0('document.getElementById("', session$ns("quiz_progress"), '").setAttribute("aria-valuenow", "', progress_pct, '");'))
    })
    
    # Handle next/prev navigation
    observeEvent(input$next_question, {
      q_idx <- current_question()
      q <- questions[[q_idx]]
      
      # Save current answer
      answers[[q$id]] <- input[[paste0("q_", q$id)]]
      
      # Move to next question
      current_question(q_idx + 1)
    })
    
    observeEvent(input$prev_question, {
      q_idx <- current_question()
      if(q_idx > 1) {
        current_question(q_idx - 1)
      }
    })
    
    # Calculate and display career matches
    output$career_matches <- renderUI({
      # A simple career matching algorithm based on answers
      
      # Process answers - this would be more sophisticated in a real implementation
      preferred_subjects <- unlist(strsplit(paste(answers$subject_preference, collapse = ","), ","))
      working_style <- answers$working_style
      baseball_interests <- answers$baseball_interest
      career_values <- answers$career_values
      technical_preference <- answers$technical_preference
      
      # Score each career based on match with answers
      career_scores <- list()
      
      for (category_name in names(careers_data)) {
        category <- careers_data[[category_name]]
        
        for (career_name in names(category)) {
          career <- category[[career_name]]
          
          score <- 0
          
          # Score based on subject match
          subject_match <- sum(career$subjects %in% preferred_subjects)
          score <- score + (subject_match * 2)  # Weight subject matches heavily
          
          # Score based on other factors (this would be more detailed in a real implementation)
          if (!is.null(baseball_interests)) {
            if ("Statistics and numbers" %in% baseball_interests && category_name == "Data & Analytics") {
              score <- score + 3
            }
            if ("The business side" %in% baseball_interests && category_name == "Legal & Business") {
              score <- score + 3
            }
            if ("Equipment and technology" %in% baseball_interests && 
                (category_name == "Technology & Media" || category_name == "Equipment & Gear")) {
              score <- score + 3
            }
            # ... and so on for other interests
          }
          
          # Technical preference
          if (!is.null(technical_preference)) {
            if (technical_preference == "Yes, I love hands-on work" && 
                (category_name == "Stadium & Facilities" || category_name == "Equipment & Gear")) {
              score <- score + 2
            }
          }
          
          # Add to scores list with category info
          career_scores[[career_name]] <- list(
            score = score,
            name = career_name,
            category = category_name,
            description = career$description,
            subjects = career$subjects
          )
        }
      }
      
      # Sort careers by score and take top 5
      career_scores <- career_scores[order(sapply(career_scores, function(x) x$score), decreasing = TRUE)]
      top_careers <- head(career_scores, 5)
      
      # Create output for top matches
      tagList(
        div(
          class = "quiz-results-intro",
          p("Based on your answers, here are the baseball careers that might be a great fit for you:")
        ),
        div(
          class = "top-careers",
          lapply(seq_along(top_careers), function(i) {
            career <- top_careers[[i]]
            
            div(
              class = "top-career-card",
              div(
                class = "match-badge",
                span(paste0("#", i))
              ),
              div(
                class = "career-header",
                h4(career$name),
                span(class = "career-category-label", career$category),
                div(
                  class = "match-score",
                  "Match Score: ",
                  div(
                    class = "progress",
                    div(
                      class = "progress-bar",
                      role = "progressbar",
                      style = paste0("width: ", min(career$score * 10, 100), "%;"),
                      paste0(min(career$score * 10, 100), "%")
                    )
                  )
                )
              ),
              div(
                class = "career-body",
                p(career$description),
                div(
                  class = "matching-subjects",
                  tags$strong("Related School Subjects:"),
                  tags$ul(
                    lapply(career$subjects, function(subject) {
                      tags$li(
                        class = ifelse(subject %in% preferred_subjects, "matched-subject", ""),
                        subject
                      )
                    })
                  )
                )
              ),
              div(
                class = "career-footer",
                actionButton(
                  session$ns(paste0("quiz_view_career_", gsub(" ", "_", career$name))),
                  "Learn More",
                  class = "btn-sm btn-info",
                  onclick = paste0("Shiny.setInputValue('", session$ns("view_career"), "', '", gsub(" ", "_", career$name), "', {priority: 'event'});")
                )
              )
            )
          })
        ),
        div(
          class = "quiz-next-steps",
          h4("What's Next?"),
          p("Click \"Learn More\" on any career to see details about education requirements and how to get started."),
          div(
            class = "quiz-actions",
            actionButton(ns("retake_quiz"), "Retake Quiz", class = "btn-secondary"),
            actionButton(ns("explore_all_careers"), "Explore All Careers", class = "btn-primary")
          )
        )
      )
    })
    
    # Handle career detail views (similar to browse component)
    observeEvent(input$view_career, {
      career_id <- input$view_career
      career_name <- gsub("_", " ", career_id)
      
      # Find the career data and show modal
      # (Same code as in the browse component)
    })
    
    # Handle retake quiz button
    observeEvent(input$retake_quiz, {
      # Reset quiz
      for(q in questions) {
        answers[[q$id]] <- NULL
      }
      current_question(1)
      shinyjs::show("question_container")
      shinyjs::show("quiz_navigation")
      shinyjs::hide("quiz_results")
    })
    
    # Handle explore all careers button
    observeEvent(input$explore_all_careers, {
      # Switch to "all careers" view
      updateRadioButtons(session$parent, "exploration_method", selected = "all")
    })
  })
}

# ===== CAREER DATA =====

#' Get Baseball Careers Data
#' 
#' @return A list of baseball careers organized by category
get_baseball_careers_data <- function() {
  # This function would ideally load from a JSON file or database
  # For now, we'll define it directly in code
  
  list(
    "Data & Analytics" = list(
      "Baseball Data Analyst" = list(
        subjects = c("Math", "Computer Science", "Statistics"),
        description = "Use advanced statistics to help teams find player strengths and weaknesses",
        education = "College degree in statistics, computer science, or math",
        skills = "Programming, data visualization, statistical analysis",
        subject_relevance = list(
          "Math" = "You'll need strong skills in algebra, statistics, and calculus to understand complex performance metrics",
          "Computer Science" = "Programming is essential for data analysis and creating statistical models",
          "Statistics" = "Understanding probability and statistical methods is the foundation of baseball analytics"
        ),
        start_now = c(
          "Take advanced math classes whenever possible",
          "Learn basic programming (Python or R are great for data analysis)",
          "Start keeping your own baseball stats during games",
          "Create your own player ranking system based on stats you think are important"
        )
      ),
      "Sports Statistician" = list(
        subjects = c("Math", "Statistics", "Research"),
        description = "Track and analyze game data to discover trends and patterns",
        education = "College degree in mathematics or statistics",
        skills = "Attention to detail, database management, analytical thinking",
        subject_relevance = list(
          "Math" = "You'll need strong mathematical skills to calculate and interpret various player metrics",
          "Statistics" = "Understanding statistical methods is crucial for analyzing performance data",
          "Research" = "Finding patterns in data requires strong research and critical thinking skills"
        ),
        start_now = c(
          "Practice keeping detailed game stats for your team",
          "Learn to use spreadsheets to organize and analyze data",
          "Read books about baseball statistics and analytics",
          "Create statistical reports on your team's performance"
        )
      ),
      "Performance Analyst" = list(
        subjects = c("Math", "Physics", "Computer Science"),
        description = "Study player movements and techniques to improve performance",
        education = "Degree in sports science or data analytics",
        skills = "Video analysis, biomechanics knowledge, coaching ability",
        subject_relevance = list(
          "Math" = "You'll use mathematical modeling to analyze player movements and techniques",
          "Physics" = "Understanding forces, momentum, and energy helps analyze athletic movements",
          "Computer Science" = "You'll need programming skills to work with specialized performance software"
        ),
        start_now = c(
          "Study the physics of baseball (how balls move, how bats work)",
          "Record and analyze your own pitching or batting technique",
          "Learn about body mechanics and how different movements affect performance",
          "Study how professional players use their bodies during different baseball actions"
        )
      )
    ),
    
    "Stadium & Facilities" = list(
      "Stadium Architect" = list(
        subjects = c("Math", "Physics", "Art", "Design"),
        description = "Design baseball stadiums that are functional, safe, and exciting",
        education = "Architecture degree + specialized sports facility training",
        skills = "3D modeling, structural engineering basics, creative design",
        subject_relevance = list(
          "Math" = "You'll use geometry and calculations for designing complex structures",
          "Physics" = "Understanding structural forces is essential for safe stadium design",
          "Art" = "Creative vision helps design aesthetically pleasing facilities",
          "Design" = "Spatial thinking helps create functional layouts for players and fans"
        ),
        start_now = c(
          "Design your dream baseball stadium using drawing or modeling software",
          "Visit different stadiums and take notes on what makes them unique",
          "Study how stadium design affects gameplay (e.g., field dimensions, wind patterns)",
          "Learn about building materials and how they're used in stadium construction"
        )
      ),
      "Master Electrician" = list(
        subjects = c("Physics", "Math", "Technical Education"),
        description = "Install and maintain complex electrical systems in stadiums",
        education = "Electrical trade school + apprenticeship",
        skills = "Technical knowledge, wiring skills, reading blueprints, safety protocols",
        subject_relevance = list(
          "Physics" = "Understanding electricity and circuits is the foundation of electrical work",
          "Math" = "You'll use calculations for load requirements, voltage drop, and circuit design",
          "Technical Education" = "Hands-on training with tools and equipment is essential"
        ),
        start_now = c(
          "Take physics classes that cover electricity and circuits",
          "Build simple electrical projects with proper supervision",
          "Learn to read electrical diagrams and schematics",
          "Join a robotics club or other technical group at school"
        )
      ),
      "Commercial Plumber" = list(
        subjects = c("Physics", "Math", "Technical Education"),
        description = "Install and maintain large-scale plumbing systems that serve thousands of fans",
        education = "Plumbing trade school + apprenticeship",
        skills = "Problem-solving, piping knowledge, blueprint reading, pressure calculations",
        subject_relevance = list(
          "Physics" = "Understanding water pressure, flow rates, and gravity feed systems",
          "Math" = "Calculating pipe sizes, angles, and system requirements",
          "Technical Education" = "Hands-on work with tools and materials"
        ),
        start_now = c(
          "Take physics classes covering fluid dynamics",
          "Build simple water systems like irrigation for a garden",
          "Learn basic plumbing repairs at home (with supervision)",
          "Study how different materials are used in modern plumbing"
        )
      ),
      "HVAC Systems Specialist" = list(
        subjects = c("Physics", "Chemistry", "Technical Education"),
        description = "Control temperature and air quality throughout massive stadium facilities",
        education = "HVAC training program + certification",
        skills = "Understanding climate control, technical repairs, preventative maintenance",
        subject_relevance = list(
          "Physics" = "Heat transfer and thermodynamics are key to HVAC system design",
          "Chemistry" = "Understanding refrigerants and air quality components",
          "Technical Education" = "Hands-on work with complex mechanical systems"
        ),
        start_now = c(
          "Study how heating and cooling systems work in buildings",
          "Take physics classes covering thermodynamics and heat transfer",
          "Learn about air flow and ventilation systems",
          "Build simple climate monitoring systems as a science project"
        )
      ),
      "Groundskeeper" = list(
        subjects = c("Biology", "Chemistry", "Environmental Science"),
        description = "Maintain perfect playing surfaces for optimal game conditions",
        education = "Turf management or horticulture programs",
        skills = "Plant science, soil management, equipment operation, weather tracking",
        subject_relevance = list(
          "Biology" = "Understanding plant growth, diseases, and health requirements",
          "Chemistry" = "Knowledge of soil composition, fertilizers, and treatments",
          "Environmental Science" = "Creating sustainable field management practices"
        ),
        start_now = c(
          "Learn about different grass types used in baseball fields",
          "Study how soil composition affects plant growth",
          "Help maintain your local baseball field",
          "Start a garden at home to learn about plant care"
        )
      )
    ),
    
    "Legal & Business" = list(
      "Sports Agent" = list(
        subjects = c("Math", "Communication", "Business", "Psychology"),
        description = "Represent players in contract negotiations and endorsement deals",
        education = "Business or law degree",
        skills = "Negotiation, relationship building, marketing knowledge",
        subject_relevance = list(
          "Math" = "Understanding contract numbers, percentages, and financial planning",
          "Communication" = "Clear and persuasive speaking and writing skills are essential",
          "Business" = "Knowledge of sports markets, marketing, and management principles",
          "Psychology" = "Understanding player motivations and negotiation psychology"
        ),
        start_now = c(
          "Practice your negotiation skills in everyday situations",
          "Learn about sports contracts and how they work",
          "Develop your speaking and writing abilities",
          "Study the business side of professional sports"
        )
      ),
      "Sports Attorney" = list(
        subjects = c("English", "Government", "Debate"),
        description = "Handle contracts, disputes, and legal matters for teams or players",
        education = "Law degree with sports law specialization",
        skills = "Critical thinking, attention to detail, persuasive communication",
        subject_relevance = list(
          "English" = "Strong reading and writing skills are the foundation of legal work",
          "Government" = "Understanding how laws and regulations affect sports",
          "Debate" = "Developing persuasive arguments and public speaking abilities"
        ),
        start_now = c(
          "Join debate club or mock trial at school",
          "Practice writing persuasive arguments",
          "Read about famous sports legal cases",
          "Learn about player contracts and league rules"
        )
      ),
      "Sports Lobbyist" = list(
        subjects = c("Government", "Communication", "Economics"),
        description = "Work with governments on stadium funding and sports legislation",
        education = "Political science or public policy degree",
        skills = "Networking, persuasion, political knowledge",
        subject_relevance = list(
          "Government" = "Understanding political processes and how laws are made",
          "Communication" = "Persuasive speaking and clear writing are essential skills",
          "Economics" = "Knowledge of public financing and economic impact analysis"
        ),
        start_now = c(
          "Follow how local governments interact with sports teams",
          "Learn about stadium financing in different cities",
          "Practice writing persuasive letters to officials",
          "Study the economic impact of sports facilities"
        )
      )
    ),
    
    "Team Operations" = list(
      "Team Scout" = list(
        subjects = c("Observation Skills", "Writing", "Math"),
        description = "Evaluate talent and identify promising players for teams",
        education = "Playing/coaching experience + scouting training",
        skills = "Player evaluation, report writing, travel management",
        subject_relevance = list(
          "Observation Skills" = "Developing an eye for talent and potential",
          "Writing" = "Creating detailed, accurate scouting reports",
          "Math" = "Understanding statistics and performance metrics"
        ),
        start_now = c(
          "Watch games critically and take notes on player skills",
          "Practice writing detailed reports about players you watch",
          "Learn about the five tools of baseball (hitting for average, hitting for power, running speed, arm strength, fielding)",
          "Study how different body types and athletic abilities translate to baseball success"
        )
      ),
      "General Manager" = list(
        subjects = c("Math", "Economics", "Psychology", "Communication"),
        description = "Build team rosters and make key personnel decisions",
        education = "Business or sports management degree",
        skills = "Negotiation, talent evaluation, strategic thinking",
        subject_relevance = list(
          "Math" = "Understanding player statistics, team finances, and contract values",
          "Economics" = "Making decisions about resource allocation and value",
          "Psychology" = "Understanding team dynamics and player motivation",
          "Communication" = "Clearly expressing vision and negotiating effectively"
        ),
        start_now = c(
          "Play fantasy baseball to practice building balanced teams",
          "Study how successful teams are constructed",
          "Learn about the baseball draft and player development",
          "Practice making strategic decisions with limited resources"
        )
      ),
      "Farm System Director" = list(
        subjects = c("Teaching", "Psychology", "Management"),
        description = "Oversee player development throughout minor league systems",
        education = "Sports management degree or coaching experience",
        skills = "Player development, organization, long-term planning",
        subject_relevance = list(
          "Teaching" = "Helping young players improve their skills efficiently",
          "Psychology" = "Understanding how to motivate and support developing players",
          "Management" = "Organizing resources across multiple teams and locations"
        ),
        start_now = c(
          "Learn about how players progress through development levels",
          "Study teaching methods for different baseball skills",
          "Help coach younger players at camps or clinics",
          "Create development plans for improving specific skills"
        )
      )
    ),
    
    "Technology & Media" = list(
      "Sports Broadcaster" = list(
        subjects = c("Public Speaking", "Writing", "Media"),
        description = "Announce and analyze games for TV, radio, or streaming",
        education = "Communications or journalism degree",
        skills = "Clear speaking, baseball knowledge, thinking on your feet",
        subject_relevance = list(
          "Public Speaking" = "Delivering clear, engaging commentary without a script",
          "Writing" = "Preparing game notes and storytelling segments",
          "Media" = "Understanding production elements and broadcast flow"
        ),
        start_now = c(
          "Practice calling games while watching them on TV",
          "Record yourself announcing plays and listen back",
          "Study professional broadcasters' techniques",
          "Learn baseball terminology and history for reference during broadcasts"
        )
      ),
      "Video Analyst" = list(
        subjects = c("Computer Science", "Physics", "Observation Skills"),
        description = "Break down game footage to help players improve",
        education = "Sports science or video production background",
        skills = "Video editing, pattern recognition, teaching ability",
        subject_relevance = list(
          "Computer Science" = "Using specialized software for video analysis",
          "Physics" = "Understanding mechanics of player movements",
          "Observation Skills" = "Identifying subtle patterns and techniques"
        ),
        start_now = c(
          "Record and analyze your own or teammates' techniques",
          "Learn basic video editing skills",
          "Study how small changes in technique affect performance",
          "Compare professional players' mechanics through video"
        )
      ),
      "Baseball App Developer" = list(
        subjects = c("Computer Science", "Math", "Design"),
        description = "Create mobile apps for fantasy baseball, stats tracking, or training",
        education = "Computer science or software engineering degree",
        skills = "Programming, user experience design, problem-solving",
        subject_relevance = list(
          "Computer Science" = "Building and maintaining software applications",
          "Math" = "Creating algorithms for statistics and analysis",
          "Design" = "Making interfaces that are easy and enjoyable to use"
        ),
        start_now = c(
          "Learn programming languages like Python or JavaScript",
          "Create a simple baseball stats tracker",
          "Design mockups for your ideal baseball app",
          "Study how popular sports apps are organized"
        )
      )
    ),
    
    "Science & Medicine" = list(
      "Sports Medicine Physician" = list(
        subjects = c("Biology", "Chemistry", "Health"),
        description = "Treat and prevent injuries for baseball players",
        education = "Medical degree with sports medicine specialization",
        skills = "Diagnosis, treatment planning, injury prevention",
        subject_relevance = list(
          "Biology" = "Understanding human anatomy and physiological processes",
          "Chemistry" = "Knowledge of biochemical processes and pharmacology",
          "Health" = "Promoting wellness and preventing injuries"
        ),
        start_now = c(
          "Take advanced science courses, especially biology",
          "Learn about common baseball injuries and prevention",
          "Study proper warmup and conditioning techniques",
          "Volunteer at sports medicine clinics if possible"
        )
      ),
      "Physical Therapist" = list(
        subjects = c("Biology", "Physics", "Health"),
        description = "Help players recover from injuries and improve movement",
        education = "Doctor of Physical Therapy degree",
        skills = "Rehabilitation techniques, anatomy knowledge, patience",
        subject_relevance = list(
          "Biology" = "Detailed knowledge of muscles, joints, and movement",
          "Physics" = "Understanding forces, leverage, and movement patterns",
          "Health" = "Promoting proper recovery and preventing re-injury"
        ),
        start_now = c(
          "Study human anatomy, especially the shoulder and arm",
          "Learn proper stretching and strengthening exercises",
          "Volunteer with athletic trainers at games if possible",
          "Research how professionals rehabilitate from common injuries"
        )
      ),
      "Biomechanics Researcher" = list(
        subjects = c("Physics", "Biology", "Math"),
        description = "Study how the body moves to improve performance and prevent injuries",
        education = "Kinesiology or biomechanics degree",
        skills = "Motion analysis, research methods, communication",
        subject_relevance = list(
          "Physics" = "Understanding forces, momentum, and energy in movement",
          "Biology" = "Knowledge of how the body's structures function together",
          "Math" = "Analyzing data and creating mathematical models of movement"
        ),
        start_now = c(
          "Study the physics of throwing and hitting",
          "Analyze videos of professional players' mechanics",
          "Learn about common injuries and their mechanical causes",
          "Conduct simple experiments on how changing a movement affects results"
        )
      ),
      "Sports Nutritionist" = list(
        subjects = c("Biology", "Chemistry", "Health"),
        description = "Design meal plans to optimize player performance and recovery",
        education = "Nutrition degree with sports specialization",
        skills = "Nutritional analysis, meal planning, education",
        subject_relevance = list(
          "Biology" = "Understanding how the body uses nutrients",
          "Chemistry" = "Knowledge of how different foods affect body chemistry",
          "Health" = "Promoting overall wellness through nutrition"
        ),
        start_now = c(
          "Study basic nutrition principles for athletes",
          "Learn how different nutrients affect performance and recovery",
          "Create sample meal plans for game days vs. training days",
          "Experiment with tracking your own nutrition and energy levels"
        )
      )
    ),
    
    "Game Day Operations" = list(
      "Stadium Security Manager" = list(
        subjects = c("Criminal Justice", "Psychology", "Communications"),
        description = "Coordinate security teams to keep fans and players safe",
        education = "Criminal justice degree or security management certification",
        skills = "Team leadership, emergency planning, conflict resolution, communication",
        subject_relevance = list(
          "Criminal Justice" = "Understanding safety protocols and legal requirements",
          "Psychology" = "Managing crowds and de-escalating tensions",
          "Communications" = "Coordinating teams and communicating clearly in emergencies"
        ),
        start_now = c(
          "Learn about crowd management techniques",
          "Study emergency response procedures for public venues",
          "Develop leadership and communication skills",
          "Volunteer to help with security at local sporting events"
        )
      ),
      "Video Production Director" = list(
        subjects = c("Media Production", "Communication", "Art"),
        description = "Create engaging video content for the scoreboard and broadcasts",
        education = "Media production degree or certification",
        skills = "Video editing, directing, storytelling, live production",
        subject_relevance = list(
          "Media Production" = "Creating professional-quality video content",
          "Communication" = "Conveying information clearly and compellingly",
          "Art" = "Developing a sense of visual composition and design"
        ),
        start_now = c(
          "Learn video editing with free software",
          "Create highlight reels of games or players",
          "Study how pro sports broadcasts are produced",
          "Experiment with different visual and audio techniques"
        )
      ),
      "Equipment Manager" = list(
        subjects = c("Organization", "Materials Science", "Management"),
        description = "Maintain and prepare all team equipment for games and practices",
        education = "Sports management + equipment manager certification",
        skills = "Organization, inventory management, equipment knowledge, problem-solving",
        subject_relevance = list(
          "Organization" = "Keeping track of thousands of pieces of equipment",
          "Materials Science" = "Understanding how different materials are used in equipment",
          "Management" = "Overseeing equipment staff and budgets"
        ),
        start_now = c(
          "Learn about different baseball equipment and materials",
          "Develop strong organizational systems",
          "Practice basic equipment maintenance and repair",
          "Study how equipment affects player performance"
        )
      )
    ),
    
    "Equipment & Gear" = list(
      "Equipment Designer" = list(
        subjects = c("Engineering", "Physics", "Design"),
        description = "Create better bats, gloves, helmets, and other baseball equipment",
        education = "Industrial design or engineering degree",
        skills = "3D modeling, materials knowledge, innovative thinking",
        subject_relevance = list(
          "Engineering" = "Building functional, durable equipment",
          "Physics" = "Understanding how forces affect equipment performance",
          "Design" = "Creating equipment that is both functional and appealing"
        ),
        start_now = c(
          "Study the physics of baseball equipment",
          "Learn about materials used in different gear",
          "Try modifying equipment (safely) to see how changes affect performance",
          "Design your own ideal baseball equipment"
        )
      ),
      "Performance Gear Specialist" = list(
        subjects = c("Material Science", "Biology", "Fashion Design"),
        description = "Develop clothing and gear that improves player comfort and performance",
        education = "Textile engineering or product design degree",
        skills = "Material testing, prototyping, user feedback integration",
        subject_relevance = list(
          "Material Science" = "Understanding fabric properties and advanced materials",
          "Biology" = "Knowledge of how the body moves and regulates temperature",
          "Fashion Design" = "Creating comfortable, functional, attractive sportswear"
        ),
        start_now = c(
          "Research different fabric technologies used in sports",
          "Learn how clothing affects athletic performance",
          "Study how weather conditions impact gear needs",
          "Design improvements to current baseball uniforms"
        )
      )
    )
  )
}