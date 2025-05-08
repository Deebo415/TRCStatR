# league_standings_widget.R
# Creates an HTML widget for displaying league standings on external websites
# This widget will update whenever the master_team_standings table is updated

# Load required packages
library(shiny)
library(htmlwidgets)
library(htmltools)
library(dplyr)
library(jsonlite)

# Function to create the standings widget
create_standings_widget <- function(
  data_path = "www/standings_data.json",
  width = "100%",
  height = "auto",
  background_color = "#800000",
  text_color = "white",
  table_background = "white",
  table_text_color = "black",
  logo_path = "www/logos/",
  auto_refresh = TRUE,
  refresh_interval = 3600  # 1 hour in seconds
) {
  
  # Create a unique ID for this widget
  widget_id <- paste0("standings-widget-", sample(1000:9999, 1))
  
  # CSS styles for the widget
  css <- paste0("
    #", widget_id, " {
      font-family: 'Arial', sans-serif;
      background-color: ", background_color, ";
      color: ", text_color, ";
      padding: 20px;
      border-radius: 5px;
      width: ", width, ";
      margin: 0 auto;
    }
    #", widget_id, " .standings-table {
      width: 100%;
      border-collapse: collapse;
      background-color: ", table_background, ";
      color: ", table_text_color, ";
      border-radius: 3px;
      overflow: hidden;
    }
    #", widget_id, " .standings-table th {
      background-color: #f2f2f2;
      padding: 10px;
      text-align: center;
      font-weight: bold;
      border-bottom: 2px solid #ddd;
    }
    #", widget_id, " .standings-table td {
      padding: 8px;
      text-align: center;
      border-bottom: 1px solid #eee;
    }
    #", widget_id, " .standings-table tr:hover {
      background-color: #f5f5f5;
    }
    #", widget_id, " .team-name {
      text-align: left;
      font-weight: bold;
    }
    #", widget_id, " .run-diff-positive {
      color: green;
      font-weight: bold;
    }
    #", widget_id, " .run-diff-negative {
      color: red;
      font-weight: bold;
    }
    #", widget_id, " .run-diff-zero {
      color: gray;
    }
    #", widget_id, " .widget-title {
      text-align: center;
      margin-bottom: 15px;
      font-size: 1.5em;
      font-weight: bold;
    }
    #", widget_id, " .widget-footer {
      text-align: center;
      margin-top: 10px;
      font-size: 0.8em;
      opacity: 0.8;
    }
    #", widget_id, " .team-logo {
      height: 40px;
      vertical-align: middle;
    }
  ")
  
  # JavaScript to load and render the standings data
  js <- paste0('
    function createStandingsWidget(elementId, dataPath, logoPath, autoRefresh, refreshInterval) {
      const widgetElement = document.getElementById(elementId);
      
      // Function to fetch and render the standings data
      function fetchAndRenderStandings() {
        fetch(dataPath)
          .then(response => response.json())
          .then(data => {
            // Sort the data by winning percentage (descending), then run differential, then runs scored
            data.sort((a, b) => {
              if (b.WinningPct !== a.WinningPct) return b.WinningPct - a.WinningPct;
              if (b.RunDifferential !== a.RunDifferential) return b.RunDifferential - a.RunDifferential;
              if (b.RunsScored !== a.RunsScored) return b.RunsScored - a.RunsScored;
              return a.RunsAgainst - b.RunsAgainst;
            });
            
            // Create the table HTML
            let tableHtml = `
              <div class="widget-title">League Standings</div>
              <table class="standings-table">
                <thead>
                  <tr>
                    <th style="width: 60px;"></th>
                    <th style="text-align: left;">Team</th>
                    <th>W</th>
                    <th>L</th>
                    <th>T</th>
                    <th>PCT</th>
                    <th>RS</th>
                    <th>RA</th>
                    <th>DIFF</th>
                  </tr>
                </thead>
                <tbody>
            `;
            
            // Add each team to the table
            data.forEach(team => {
              const runDiffClass = team.RunDifferential > 0 ? "run-diff-positive" : 
                                  (team.RunDifferential < 0 ? "run-diff-negative" : "run-diff-zero");
              const runDiffDisplay = team.RunDifferential > 0 ? `+${team.RunDifferential}` : team.RunDifferential;
              const logoFilename = team.LogoFilename || team.TeamName.toLowerCase().replace(/ /g, "_");
              
              tableHtml += `
                <tr>
                  <td style="text-align: center;">
                    <img src="${logoPath}${logoFilename}.png" class="team-logo" alt="${team.TeamName} logo">
                  </td>
                  <td class="team-name">${team.TeamName}</td>
                  <td>${team.Wins}</td>
                  <td>${team.Losses}</td>
                  <td>${team.Ties}</td>
                  <td>${team.PCT}</td>
                  <td>${team.RunsScored}</td>
                  <td>${team.RunsAgainst}</td>
                  <td class="${runDiffClass}">${runDiffDisplay}</td>
                </tr>
              `;
            });
            
            tableHtml += `
                </tbody>
              </table>
              <div class="widget-footer">Last updated: ${new Date().toLocaleString()}</div>
            `;
            
            // Update the widget content
            widgetElement.innerHTML = tableHtml;
          })
          .catch(error => {
            widgetElement.innerHTML = `<div class="alert alert-danger">Error loading standings data: ${error.message}</div>`;
            console.error("Error loading standings data:", error);
          });
      }
      
      // Initial render
      fetchAndRenderStandings();
      
      // Set up auto-refresh if enabled
      if (autoRefresh && refreshInterval > 0) {
        setInterval(fetchAndRenderStandings, refreshInterval * 1000);
      }
    }
    
    // Initialize the widget when the page loads
    document.addEventListener("DOMContentLoaded", function() {
      createStandingsWidget("' + widget_id + '", "' + data_path + '", "' + logo_path + '", ' + 
      tolower(auto_refresh) + ', ' + refresh_interval + ');
    });
  ')
  
  # Create the HTML widget
  html <- tags$div(
    id = widget_id,
    class = "trc-standings-widget",
    tags$style(HTML(css)),
    tags$script(HTML(js)),
    tags$div(
      class = "widget-loading",
      "Loading standings data..."
    )
  )
  
  return(html)
}

# Function to export the current standings data to JSON
export_standings_data <- function(output_path = "www/standings_data.json") {
  # Check if master_team_standings exists
  if (!exists("master_team_standings", envir = .GlobalEnv)) {
    stop("master_team_standings not found in global environment")
  }
  
  # Get the master team standings
  master_standings <- get("master_team_standings", envir = .GlobalEnv)
  
  if (is.null(master_standings) || nrow(master_standings) == 0) {
    stop("master_team_standings exists but is empty")
  }
  
  # Create a standings data frame with the required columns
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
    WinningPct = as.numeric(master_standings$PCT),
    PCT = sprintf("%.3f", as.numeric(master_standings$PCT)),
    stringsAsFactors = FALSE
  )
  
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
  
  # Ensure the directory exists
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  
  # Write the standings data to JSON
  write_json(standings, output_path, pretty = TRUE)
  
  message("Standings data exported to ", output_path)
  
  return(output_path)
}

# Function to generate the HTML file with the embedded widget
generate_standings_html <- function(
  output_path = "www/standings.html",
  data_path = "standings_data.json",
  logo_path = "logos/",
  title = "TRC Baseball League Standings",
  width = "100%",
  height = "auto",
  background_color = "#800000",
  text_color = "white",
  auto_refresh = TRUE,
  refresh_interval = 3600  # 1 hour in seconds
) {
  # Create the widget
  widget <- create_standings_widget(
    data_path = data_path,
    width = width,
    height = height,
    background_color = background_color,
    text_color = text_color,
    logo_path = logo_path,
    auto_refresh = auto_refresh,
    refresh_interval = refresh_interval
  )
  
  # Create the HTML page
  html <- tags$html(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
      tags$title(title),
      tags$style(HTML("
        body {
          font-family: 'Arial', sans-serif;
          margin: 0;
          padding: 20px;
          background-color: #f5f5f5;
        }
        .container {
          max-width: 1000px;
          margin: 0 auto;
        }
        .header {
          text-align: center;
          margin-bottom: 20px;
        }
        .header h1 {
          color: #800000;
          margin-bottom: 5px;
        }
        .header p {
          color: #666;
          margin-top: 0;
        }
        .footer {
          text-align: center;
          margin-top: 20px;
          color: #666;
          font-size: 0.8em;
        }
      "))
    ),
    tags$body(
      tags$div(
        class = "container",
        tags$div(
          class = "header",
          tags$h1(title),
          tags$p("Powered by TRCStatR")
        ),
        widget,
        tags$div(
          class = "footer",
          paste("© ", format(Sys.Date(), "%Y"), " TRC Baseball. All rights reserved.")
        )
      )
    )
  )
  
  # Write the HTML to file
  html_content <- as.character(html)
  writeLines(html_content, output_path)
  
  message("Standings HTML generated at ", output_path)
  
  return(output_path)
}

# Function to generate the embed code for the widget
generate_embed_code <- function(
  widget_url = "https://example.com/standings.html",
  width = "100%",
  height = "600px"
) {
  embed_code <- paste0(
    '<iframe src="', widget_url, '" ',
    'width="', width, '" ',
    'height="', height, '" ',
    'frameborder="0" scrolling="no"></iframe>'
  )
  
  return(embed_code)
}

# Add a hook to automatically update the standings data when the app runs
if (exists("master_team_standings", envir = .GlobalEnv)) {
  tryCatch({
    export_standings_data()
    message("Standings data automatically exported on startup")
  }, error = function(e) {
    message("Error exporting standings data: ", e$message)
  })
}

# Example usage:
# 1. Export the standings data to JSON
# export_standings_data()
# 
# 2. Generate the HTML file with the embedded widget
# generate_standings_html()
# 
# 3. Get the embed code to paste into the Rec Council website
# cat(generate_embed_code("https://yourserver.com/standings.html"))
