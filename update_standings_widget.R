# update_standings_widget.R
# This script updates the standings widget and generates the embed code
# Run this script after updating your data to refresh the widget

# First, refresh the master_team_standings to ensure it's up to date
source("refresh_standings.R")

# Source the widget functions
source("league_standings_widget.R")

# Create necessary directories if they don't exist
dir.create("www", showWarnings = FALSE)
dir.create("www/logos", showWarnings = FALSE)

# Copy team logos to the www/logos directory if they're not already there
logo_files <- list.files("www", pattern = "\\.png$", full.names = TRUE)
for (logo_file in logo_files) {
  file_name <- basename(logo_file)
  if (!file.exists(file.path("www/logos", file_name))) {
    file.copy(logo_file, file.path("www/logos", file_name))
  }
}

# Export the current standings data to JSON
tryCatch({
  json_path <- export_standings_data("www/standings_data.json")
  cat("✓ Standings data exported to", json_path, "\n")
}, error = function(e) {
  cat("✗ Error exporting standings data:", e$message, "\n")
})

# Generate the HTML file with the embedded widget
tryCatch({
  html_path <- generate_standings_html(
    output_path = "www/standings.html",
    data_path = "standings_data.json",
    logo_path = "logos/",
    title = "TRC Baseball League Standings"
  )
  cat("✓ Standings HTML generated at", html_path, "\n")
}, error = function(e) {
  cat("✗ Error generating standings HTML:", e$message, "\n")
})

# Print the embed code for the Rec Council website
cat("\n=== EMBED CODE FOR REC COUNCIL WEBSITE ===\n")
cat(generate_embed_code(
  widget_url = "YOUR_SERVER_URL/standings.html", # Replace with your actual URL
  width = "100%",
  height = "600px"
))
cat("\n==========================================\n")
cat("\nInstructions:\n")
cat("1. Replace 'YOUR_SERVER_URL' in the embed code with the actual URL where your standings.html file is hosted\n")
cat("2. Copy the embed code and paste it into the Rec Council website's HTML editor\n")
cat("3. Run this script whenever you update your standings data to refresh the widget\n")

# Provide instructions for hosting
cat("\n=== HOSTING OPTIONS ===\n")
cat("Option 1: Host on your own web server\n")
cat("  - Upload the 'www' folder to your web server\n")
cat("  - The embed code URL should point to the location of standings.html on your server\n\n")
cat("Option 2: Host on GitHub Pages\n")
cat("  - Create a GitHub repository\n")
cat("  - Push the 'www' folder to the repository\n")
cat("  - Enable GitHub Pages in the repository settings\n")
cat("  - The embed code URL would be: https://YOUR_USERNAME.github.io/YOUR_REPO/standings.html\n\n")
cat("Option 3: Host on Netlify/Vercel (Free options)\n")
cat("  - Sign up for a free account at netlify.com or vercel.com\n")
cat("  - Deploy the 'www' folder to your account\n")
cat("  - The embed code URL would be provided by the service\n")
