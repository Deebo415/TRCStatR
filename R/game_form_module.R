# game_form_module.R
# Modularized Game Entry / Edit / View Shiny module for TRCStatR

library(shiny)

# UI function for the Game Form Module
# mode: 'entry', 'edit', or 'view'
game_form_module_ui <- function(id, mode = "entry") {
  ns <- NS(id)
  tagList(
    textInput(ns("game_date"), "Game Date", value = "", placeholder = "YYYY-MM-DD", disabled = (mode == "view")),
    selectInput(ns("home_team"), "Home Team", choices = NULL, selected = NULL, disabled = (mode == "view")),
    selectInput(ns("away_team"), "Away Team", choices = NULL, selected = NULL, disabled = (mode == "view")),
    textInput(ns("location"), "Location", value = "", disabled = (mode == "view")),
    textAreaInput(ns("notes"), "Notes", value = "", disabled = (mode == "view")),
    # Add more fields as needed (e.g., umpires, weather, etc.)
    if (mode != "view") actionButton(ns("save_game"), ifelse(mode == "entry", "Create Game", "Save Changes"))
  )
}

# Server function for the Game Form Module
# mode: 'entry', 'edit', or 'view'
# game_id: NULL for new game, or existing GameID for edit/view
# teams: data.frame of teams
# games: data.frame of games
# on_save: callback function to handle save (function(game_data, game_id = NULL))
game_form_module_server <- function(id, mode = "entry", game_id = NULL, teams = NULL, games = NULL, on_save = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Populate team choices
    observe({
      if (!is.null(teams)) {
        team_choices <- unique(teams$TeamName)
        updateSelectInput(session, "home_team", choices = team_choices)
        updateSelectInput(session, "away_team", choices = team_choices)
      }
    })

    # Pre-fill fields if editing or viewing
    observeEvent(game_id, {
      if (!is.null(game_id) && mode %in% c("edit", "view") && !is.null(games)) {
        game <- games[games$GameID == game_id, ]
        if (nrow(game) == 1) {
          updateTextInput(session, "game_date", value = as.character(game$GameDate))
          updateSelectInput(session, "home_team", selected = game$HomeTeam)
          updateSelectInput(session, "away_team", selected = game$AwayTeam)
          updateTextInput(session, "location", value = game$Location)
          updateTextAreaInput(session, "notes", value = game$Notes)
        }
      }
    }, ignoreInit = TRUE)

    # Save logic
    observeEvent(input$save_game, {
      # Validate and collect input
      game_data <- list(
        GameDate = input$game_date,
        HomeTeam = input$home_team,
        AwayTeam = input$away_team,
        Location = input$location,
        Notes = input$notes
        # Add more fields as needed
      )
      if (mode == "entry") {
        if (!is.null(on_save)) on_save(game_data)
      } else if (mode == "edit") {
        if (!is.null(on_save)) on_save(game_data, game_id)
      }
    })
  })
}
