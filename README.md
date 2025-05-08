# TRCStatR

TRCStatR is a Shiny application for managing and displaying baseball statistics for youth leagues. It provides comprehensive statistical analysis, team standings, player cards, and advanced metrics designed specifically for youth baseball.

## Features

- **League Standings**: Track team records, runs scored/against, and winning percentages
- **Player Statistics**: Comprehensive batting and pitching statistics for all players
- **Advanced Metrics**: Age-appropriate advanced statistics like wRC+, FIP, and WAR
- **Team Snapshots**: Quick overview of team performance and top players
- **Embeddable Widgets**: Display league standings on external websites

## Project Structure

```
TRCStatR/
├── app.R                  # Main Shiny app entry point
├── R/                     # All R modules and functions
├── www/                   # Static assets (CSS, images, JS)
│   ├── logos/             # Team logos
├── widgets/               # Standalone widgets
│   ├── standings/         # Standings widget files
├── scripts/               # Utility scripts
└── README.md              # Project documentation
```

## Installation

### Prerequisites

- R (version 4.0.0 or higher)
- RStudio (recommended for development)

### Required Packages

```r
install.packages(c(
  "shiny", 
  "shinydashboard", 
  "DT", 
  "dplyr", 
  "ggplot2", 
  "htmltools", 
  "jsonlite", 
  "htmlwidgets"
))
```

### Running the Application

1. Clone this repository
2. Open the project in RStudio
3. Run `shiny::runApp()`

## Widgets

TRCStatR includes embeddable widgets that can be used on external websites:

### League Standings Widget

The League Standings widget displays the current team standings and automatically updates when the standings change.

To use the widget:

1. Run the update script: `source("scripts/update_widgets.R")`
2. Host the generated files in the `widgets/standings/` directory
3. Embed the widget using an iframe:

```html
<iframe src="https://your-server.com/standings.html" width="100%" height="600px" frameborder="0" scrolling="no"></iframe>
```

## Deployment

### Local Development

For local development, simply run the app in RStudio using `shiny::runApp()`.

### Shinyapps.io Deployment

To deploy to shinyapps.io:

1. Install the rsconnect package: `install.packages("rsconnect")`
2. Configure your account: `rsconnect::setAccountInfo()`
3. Deploy the app: `rsconnect::deployApp()`

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Developed for the Towson Recreation Council Baseball League
- Special thanks to all the coaches and players who provided feedback
