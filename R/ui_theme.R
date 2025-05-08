# TRCStatR - UI Theme Module
# This file contains theme definitions and UI utilities for a consistent, youth-friendly interface

# Load required modules
library(shiny)
library(shinydashboard)
source("R/error_handling.R")

#' Create a custom theme for the TRCStatR application
#' 
#' @param primary_color Primary color for the application
#' @param secondary_color Secondary color for the application
#' @param accent_color Accent color for highlights and important elements
#' @param text_color Main text color
#' @param background_color Background color
#' @return A list containing the theme configuration
create_trc_theme <- function(
  primary_color = "#800000",    # Maroon
  secondary_color = "#FFFFE4",  # Off-White
  accent_color = "#AB4E52",  # Lighter Maroon
  text_color = "#212121",       # Dark grey
  background_color = "#FFFFFF"  # White
) {
  return(list(
    primary = primary_color,
    secondary = secondary_color,
    accent = accent_color,
    text = text_color,
    background = background_color,
    
    # Derived colors
    primary_light = lighten_color(primary_color, 0.3),
    primary_dark = darken_color(primary_color, 0.2),
    secondary_light = lighten_color(secondary_color, 0.3),
    secondary_dark = darken_color(secondary_color, 0.2),
    
    # Font settings
    header_font = "'Montserrat', sans-serif",
    body_font = "'Open Sans', sans-serif",
    
    # Sizes
    header_size = "1.8rem",
    subheader_size = "1.4rem",
    body_size = "1rem",
    
    # Border radius
    border_radius = "8px",
    
    # Spacing
    spacing_unit = "16px"
  ))
}

#' Create a team-specific theme
#' 
#' @param team_name Name of the team
#' @param team_colors List with primary and secondary colors
#' @return A theme configuration for the team
create_team_theme <- function(team_name, team_colors = NULL) {
  # Default team colors if not provided
  if (is.null(team_colors)) {
    # Use the team name to generate consistent colors
    team_hash <- sum(utf8ToInt(team_name)) %% 360
    primary_hue <- team_hash
    secondary_hue <- (team_hash + 120) %% 360
    
    primary_color <- hsv_to_hex(primary_hue/360, 0.7, 0.8)
    secondary_color <- hsv_to_hex(secondary_hue/360, 0.7, 0.8)
    
    team_colors <- list(primary = primary_color, secondary = secondary_color)
  }
  
  # Create a theme with the team colors
  theme <- create_trc_theme(
    primary_color = team_colors$primary,
    secondary_color = team_colors$secondary
  )
  
  # Add team-specific properties
  theme$team_name <- team_name
  theme$logo_url <- paste0("logos/", gsub("[^a-zA-Z0-9]", "", tolower(team_name)), ".png")
  
  return(theme)
}

#' Apply the theme to a Shiny UI
#' 
#' @param ui The Shiny UI to apply the theme to
#' @param theme The theme configuration
#' @return The themed UI
apply_theme <- function(ui, theme) {
  # Add CSS variables
  css <- paste0(
    ":root {",
    "  --primary-color: ", theme$primary, ";",
    "  --primary-light: ", theme$primary_light, ";",
    "  --primary-dark: ", theme$primary_dark, ";",
    "  --secondary-color: ", theme$secondary, ";",
    "  --secondary-light: ", theme$secondary_light, ";",
    "  --secondary-dark: ", theme$secondary_dark, ";",
    "  --accent-color: ", theme$accent, ";",
    "  --text-color: ", theme$text, ";",
    "  --background-color: ", theme$background, ";",
    "  --header-font: ", theme$header_font, ";",
    "  --body-font: ", theme$body_font, ";",
    "  --header-size: ", theme$header_size, ";",
    "  --subheader-size: ", theme$subheader_size, ";",
    "  --body-size: ", theme$body_size, ";",
    "  --border-radius: ", theme$border_radius, ";",
    "  --spacing-unit: ", theme$spacing_unit, ";",
    "}"
  )
  
  # Add Google Fonts
  fonts <- tags$link(
    href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&family=Open+Sans:wght@400;600&display=swap",
    rel = "stylesheet"
  )
  
  # Add Font Awesome for icons
  font_awesome <- tags$link(
    href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css",
    rel = "stylesheet"
  )
  
  # Define basic CSS inline instead of reading from file
  basic_css <- paste0(
    "/* Basic styles */\n",
    ".trc-card { background-color: white; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); margin-bottom: 16px; overflow: hidden; }\n",
    ".card-header { background-color: ", theme$primary, "; color: white; padding: 12px 16px; display: flex; align-items: center; }\n",
    ".card-header i { margin-right: 8px; font-size: 1.2em; }\n",
    ".card-header h3 { margin: 0; color: white; }\n",
    ".card-content { padding: 16px; }\n",
    ".stat-display { flex: 1 0 25%; min-width: 100px; padding: 8px; text-align: center; }\n",
    ".stat-value { font-size: 1.8em; font-weight: bold; color: ", theme$primary_dark, "; line-height: 1.2; }\n",
    ".stat-label { font-size: 0.9em; color: #212121; opacity: 0.8; }\n",
    ".trc-button { border-radius: 8px; border: none; padding: 8px 16px; font-weight: 600; cursor: pointer; display: inline-flex; align-items: center; justify-content: center; }\n",
    ".trc-button.primary { background-color: ", theme$primary, "; color: white; }\n",
    ".trc-button.secondary { background-color: ", theme$secondary, "; color: white; }\n",
    ".trc-button.accent { background-color: ", theme$accent, "; color: white; }\n"
  )
  
  # Wrap the UI with the theme
  return(
    tagList(
      tags$head(
        fonts,
        font_awesome,
        tags$style(HTML(css)),
        tags$style(HTML(basic_css))
      ),
      ui
    )
  )
}

#' Create a styled card for displaying content
#' 
#' @param title Card title
#' @param content Card content
#' @param icon Optional icon for the card
#' @param theme Theme configuration
#' @return A div containing the styled card
create_card <- function(title, content, icon = NULL, theme = NULL) {
  if (is.null(theme)) {
    theme <- create_trc_theme()
  }
  
  # Create the card header with optional icon
  if (!is.null(icon)) {
    header <- div(
      class = "card-header",
      tags$i(class = paste0("fas fa-", icon)),
      h3(title)
    )
  } else {
    header <- div(
      class = "card-header",
      h3(title)
    )
  }
  
  # Create the card
  div(
    class = "trc-card",
    header,
    div(
      class = "card-content",
      content
    )
  )
}

#' Create a styled stat display
#' 
#' @param label Stat label
#' @param value Stat value
#' @param icon Optional icon for the stat
#' @param theme Theme configuration
#' @return A div containing the styled stat
create_stat_display <- function(label, value, icon = NULL, theme = NULL) {
  if (is.null(theme)) {
    theme <- create_trc_theme()
  }
  
  # Create the stat with optional icon
  if (!is.null(icon)) {
    div(
      class = "stat-display",
      div(
        class = "stat-icon",
        tags$i(class = paste0("fas fa-", icon))
      ),
      div(
        class = "stat-content",
        div(class = "stat-value", value),
        div(class = "stat-label", label)
      )
    )
  } else {
    div(
      class = "stat-display",
      div(
        class = "stat-content",
        div(class = "stat-value", value),
        div(class = "stat-label", label)
      )
    )
  }
}

#' Create a styled player card
#' 
#' @param player_name Player name
#' @param jersey_number Jersey number
#' @param team_name Team name
#' @param stats List of stats to display
#' @param theme Theme configuration
#' @return A div containing the styled player card
create_player_card <- function(player_name, jersey_number, team_name, stats, theme = NULL) {
  if (is.null(theme)) {
    theme <- create_trc_theme()
  }
  
  # Create the player card
  div(
    class = "player-card",
    div(
      class = "player-header",
      div(
        class = "player-number",
        jersey_number
      ),
      div(
        class = "player-info",
        h3(class = "player-name", player_name),
        p(class = "player-team", team_name)
      )
    ),
    div(
      class = "player-stats",
      lapply(names(stats), function(stat_name) {
        create_stat_display(stat_name, stats[[stat_name]])
      })
    )
  )
}

#' Create a styled data table
#' 
#' @param data Data frame to display
#' @param theme Theme configuration
#' @return A div containing the styled data table
create_data_table <- function(data, theme = NULL) {
  if (is.null(theme)) {
    theme <- create_trc_theme()
  }
  
  # Create the data table with responsive design
  div(
    class = "trc-table-container",
    tags$table(
      class = "trc-table",
      tags$thead(
        tags$tr(
          lapply(names(data), function(col_name) {
            tags$th(col_name)
          })
        )
      ),
      tags$tbody(
        lapply(1:nrow(data), function(row) {
          tags$tr(
            lapply(1:ncol(data), function(col) {
              tags$td(data[row, col])
            })
          )
        })
      )
    )
  )
}

#' Create a styled button
#' 
#' @param id Button ID
#' @param label Button label
#' @param icon Optional icon for the button
#' @param style Button style (primary, secondary, accent)
#' @param theme Theme configuration
#' @return A styled button
create_button <- function(id, label, icon = NULL, style = "primary", theme = NULL) {
  if (is.null(theme)) {
    theme <- create_trc_theme()
  }
  
  # Create button classes
  btn_class <- paste0("trc-button ", style)
  
  # Create the button with optional icon
  if (!is.null(icon)) {
    actionButton(
      id,
      label = span(
        tags$i(class = paste0("fas fa-", icon)),
        label
      ),
      class = btn_class
    )
  } else {
    actionButton(
      id,
      label = label,
      class = btn_class
    )
  }
}

#' Create a navigation bar
#' 
#' @param title App title
#' @param menu_items List of menu items (id, label, icon)
#' @param theme Theme configuration
#' @return A div containing the navigation bar
create_navbar <- function(title, menu_items, theme = NULL) {
  if (is.null(theme)) {
    theme <- create_trc_theme()
  }
  
  # Create the navigation bar
  div(
    class = "trc-navbar",
    div(
      class = "navbar-title",
      h1(title)
    ),
    div(
      class = "navbar-menu",
      lapply(menu_items, function(item) {
        div(
          class = "menu-item",
          actionLink(
            item$id,
            label = span(
              tags$i(class = paste0("fas fa-", item$icon)),
              item$label
            )
          )
        )
      })
    )
  )
}

# Helper functions for color manipulation

#' Lighten a color by a factor
#' 
#' @param color Color in hex format
#' @param factor Factor to lighten by (0-1)
#' @return Lightened color in hex format
lighten_color <- function(color, factor) {
  # Convert hex to RGB
  r <- strtoi(substr(color, 2, 3), 16)
  g <- strtoi(substr(color, 4, 5), 16)
  b <- strtoi(substr(color, 6, 7), 16)
  
  # Lighten
  r <- min(255, r + (255 - r) * factor)
  g <- min(255, g + (255 - g) * factor)
  b <- min(255, b + (255 - b) * factor)
  
  # Convert back to hex
  return(sprintf("#%02X%02X%02X", round(r), round(g), round(b)))
}

#' Darken a color by a factor
#' 
#' @param color Color in hex format
#' @param factor Factor to darken by (0-1)
#' @return Darkened color in hex format
darken_color <- function(color, factor) {
  # Convert hex to RGB
  r <- strtoi(substr(color, 2, 3), 16)
  g <- strtoi(substr(color, 4, 5), 16)
  b <- strtoi(substr(color, 6, 7), 16)
  
  # Darken
  r <- max(0, r * (1 - factor))
  g <- max(0, g * (1 - factor))
  b <- max(0, b * (1 - factor))
  
  # Convert back to hex
  return(sprintf("#%02X%02X%02X", round(r), round(g), round(b)))
}

#' Convert HSV to hex color
#' 
#' @param h Hue (0-1)
#' @param s Saturation (0-1)
#' @param v Value (0-1)
#' @return Hex color string
hsv_to_hex <- function(h, s, v) {
  # HSV to RGB conversion
  c <- v * s
  x <- c * (1 - abs((h * 6) %% 2 - 1))
  m <- v - c
  
  # RGB based on hue
  if (h < 1/6) {
    r <- c; g <- x; b <- 0
  } else if (h < 2/6) {
    r <- x; g <- c; b <- 0
  } else if (h < 3/6) {
    r <- 0; g <- c; b <- x
  } else if (h < 4/6) {
    r <- 0; g <- x; b <- c
  } else if (h < 5/6) {
    r <- x; g <- 0; b <- c
  } else {
    r <- c; g <- 0; b <- x
  }
  
  # Adjust RGB values
  r <- (r + m) * 255
  g <- (g + m) * 255
  b <- (b + m) * 255
  
  # Convert to hex
  return(sprintf("#%02X%02X%02X", round(r), round(g), round(b)))
}

#' Get inline CSS for a theme
#' 
#' @param theme Theme configuration
#' @return CSS content as a string
get_inline_css <- function(theme) {
  return(paste0(
    "/* Basic styles */\n",
    ".trc-card { background-color: white; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); margin-bottom: 16px; overflow: hidden; }\n",
    ".card-header { background-color: ", theme$primary, "; color: white; padding: 12px 16px; display: flex; align-items: center; }\n",
    ".card-header i { margin-right: 8px; font-size: 1.2em; }\n",
    ".card-header h3 { margin: 0; color: white; }\n",
    ".card-content { padding: 16px; }\n",
    ".stat-display { flex: 1 0 25%; min-width: 100px; padding: 8px; text-align: center; }\n",
    ".stat-value { font-size: 1.8em; font-weight: bold; color: ", theme$primary_dark, "; line-height: 1.2; }\n",
    ".stat-label { font-size: 0.9em; color: #212121; opacity: 0.8; }\n",
    ".trc-button { border-radius: 8px; border: none; padding: 8px 16px; font-weight: 600; cursor: pointer; display: inline-flex; align-items: center; justify-content: center; }\n",
    ".trc-button.primary { background-color: ", theme$primary, "; color: white; }\n",
    ".trc-button.secondary { background-color: ", theme$secondary, "; color: white; }\n",
    ".trc-button.accent { background-color: ", theme$accent, "; color: white; }\n"
  ))
}
