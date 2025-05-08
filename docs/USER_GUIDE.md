# TRCStatR User Guide

## Introduction

Welcome to TRCStatR, a baseball statistics application designed specifically for youth baseball players (ages 11-13) and their coaches. This guide will help you navigate the application and make the most of its features.

## Getting Started

### Launching the Application

1. Open RStudio
2. Navigate to the TRCStatR project folder
3. Open the `app.R` file
4. Click the "Run App" button in RStudio or run `shiny::runApp()` in the console
5. The application will open in your default web browser

### Navigation

The application has a sidebar menu with the following options:

- **Game Entry**: Record statistics for new games
- **View/Edit Games**: View and edit existing game data
- **Players**: Manage player information
- **Teams**: Manage team information
- **Reports**: Generate and view reports
- **Advanced Metrics**: Explore advanced baseball statistics

## Game Entry

The Game Entry tab allows you to record statistics for a new game.

### Step 1: Select Teams

1. Select the home team from the "Home Team" dropdown
2. Select the away team from the "Away Team" dropdown
3. Select the game date using the date picker
4. Click "Initialize Game" to set up the game

### Step 2: Record Batting Statistics

For each team (home and away):

1. Select a player from the dropdown list
2. Enter the player's batting statistics:
   - PA (Plate Appearances)
   - AB (At Bats)
   - H (Hits)
   - 2B (Doubles)
   - 3B (Triples)
   - HR (Home Runs)
   - BB (Walks)
   - SO (Strikeouts)
   - HBP (Hit By Pitch)
   - SF (Sacrifice Flies)
   - SH (Sacrifice Hits)
   - SB (Stolen Bases)
   - CS (Caught Stealing)
   - ROE (Reached On Error)
   - RBI (Runs Batted In)
3. Click "Add Batter" to add another player
4. The system automatically calculates AVG (Batting Average) and OBP (On-Base Percentage)

### Step 3: Record Pitching Statistics

For each team (home and away):

1. Select a player from the dropdown list
2. Enter the player's pitching statistics:
   - IP (Innings Pitched)
   - H (Hits Allowed)
   - R (Runs Allowed)
   - ER (Earned Runs)
   - BB (Walks)
   - SO (Strikeouts)
   - HBP (Hit Batters)
   - BF (Batters Faced)
   - Pitches (Total Pitches)
   - Strikes (Strikes Thrown)
3. Click "Add Pitcher" to add another player
4. The system automatically calculates ERA (Earned Run Average) and WHIP (Walks plus Hits per Inning Pitched)

### Step 4: Save Game

1. Review all entered statistics for accuracy
2. Click "Save Game" to save the game to the database
3. A confirmation message will appear when the game is successfully saved

## View/Edit Games

The View/Edit Games tab allows you to view and edit existing game data.

### Viewing Games

1. Use the filters to find specific games:
   - Date Range: Select start and end dates
   - Team: Select a specific team
   - Status: Select "Complete" or "Incomplete" games
2. Click "Apply Filters" to update the game list
3. The game list shows basic information for each game:
   - Game Date
   - Home Team
   - Away Team
   - Score
   - Status

### Editing Games

1. Select a game from the list
2. Click "Edit Game" to open the game editor
3. Make necessary changes to batting and pitching statistics
4. Click "Update Game" to save your changes
5. A confirmation message will appear when the game is successfully updated

## Players

The Players tab allows you to manage player information.

### Viewing Players

1. Use the filters to find specific players:
   - Team: Select a specific team
   - Position: Select a specific position
   - Name: Enter a player's name
2. Click "Apply Filters" to update the player list
3. The player list shows basic information for each player:
   - Name
   - Team
   - Position
   - Jersey Number
   - Age

### Adding Players

1. Click "Add New Player" to open the player form
2. Enter the player's information:
   - Name
   - Team
   - Position
   - Jersey Number
   - Date of Birth
   - Bats (Left/Right/Switch)
   - Throws (Left/Right)
   - Additional Notes
3. Click "Save Player" to add the player to the database
4. A confirmation message will appear when the player is successfully added

### Editing Players

1. Select a player from the list
2. Click "Edit Player" to open the player editor
3. Make necessary changes to the player's information
4. Click "Update Player" to save your changes
5. A confirmation message will appear when the player is successfully updated

## Teams

The Teams tab allows you to manage team information.

### Viewing Teams

1. The team list shows all teams in the database with basic information:
   - Team Name
   - Coach
   - League
   - Number of Players

### Adding Teams

1. Click "Add New Team" to open the team form
2. Enter the team's information:
   - Team Name
   - Coach Name
   - Coach Contact
   - League
   - Team Colors
   - Additional Notes
3. Click "Save Team" to add the team to the database
4. A confirmation message will appear when the team is successfully added

### Editing Teams

1. Select a team from the list
2. Click "Edit Team" to open the team editor
3. Make necessary changes to the team's information
4. Click "Update Team" to save your changes
5. A confirmation message will appear when the team is successfully updated

### Team Roster

1. Select a team from the list
2. Click "View Roster" to see all players on the team
3. The roster shows each player's:
   - Name
   - Position
   - Jersey Number
   - Key Statistics

## Reports

The Reports tab allows you to generate and view various reports.

### Team Reports

1. Select a team from the dropdown
2. Click "Load Report" to generate the team report
3. The team report includes:
   - Team Summary (wins, losses, runs scored, runs allowed)
   - Team Batting Statistics
   - Team Pitching Statistics
   - Game Results
   - Trends and Graphs

### Player Cards

1. Select a team from the dropdown (or "All Teams")
2. Select a player from the dropdown
3. Click "Load Player Card" to generate the player card
4. The player card includes:
   - Player Information
   - Season Statistics
   - Game-by-Game Performance
   - Trends and Graphs
   - Comparison to Team Averages

### Leaderboards

1. Select a statistic from the dropdown (AVG, HR, RBI, OPS, ERA, etc.)
2. Enter the number of players to display
3. Click "Load Leaderboard" to generate the leaderboard
4. The leaderboard shows the top players for the selected statistic

### Interactive Visualizations

The Interactive Visualizations section allows you to create custom charts and graphs:

1. Select the visualization type:
   - Player Comparison
   - Team Performance
   - Batting Statistics
   - Pitching Statistics
2. Select the specific data to display
3. The visualization will update automatically
4. Hover over elements to see detailed information
5. Use the controls to adjust the visualization

## Advanced Metrics

The Advanced Metrics tab allows you to explore advanced baseball statistics with educational explanations.

### Exploring Metrics

1. Select the player type (Batter or Pitcher)
2. Select a player from the dropdown
3. Click "Load Advanced Metrics" to view the player's advanced metrics
4. The metrics are displayed in an easy-to-understand format with:
   - Visual indicators of performance
   - Explanations of what each metric means
   - Comparisons to league averages

### Understanding Advanced Metrics

The Advanced Metrics tab includes educational content to help young players understand complex statistics:

#### For Batters:
- **wOBA (Weighted On-Base Average)**: Like OBP, but gives proper credit to each way of reaching base
- **wRC+ (Weighted Runs Created Plus)**: Measures how many runs a player creates compared to league average
- **OPS+ (On-Base Plus Slugging Plus)**: Compares a player's OPS to league average
- **WAR (Wins Above Replacement)**: Estimates how many more wins a player provides compared to a replacement player
- **YPDI (Youth Player Development Index)**: A special stat for young players that measures both performance and development skills

#### For Pitchers:
- **FIP (Fielding Independent Pitching)**: Measures a pitcher's effectiveness at preventing home runs, walks, and hit batters, and getting strikeouts
- **WAR (Wins Above Replacement)**: Estimates how many more wins a pitcher provides compared to a replacement player
- **PER (Pitcher Effectiveness Rating)**: A special stat for young pitchers that combines performance with development skills

## Tips and Best Practices

### Data Entry
- Enter game data as soon as possible after games
- Double-check all statistics before saving
- Use the auto-calculated stats (AVG, OBP, ERA, WHIP) to verify your entries

### Using Reports
- Use team reports to identify strengths and areas for improvement
- Use player cards to track individual development
- Use leaderboards to recognize top performers
- Use visualizations to identify trends and patterns

### Understanding Statistics
- Start with basic statistics (AVG, ERA) before moving to advanced metrics
- Use the explanations provided in the Advanced Metrics tab
- Compare players to team and league averages, not just to each other
- Focus on development trends over time, not just current performance

## Troubleshooting

### Common Issues

#### Application Won't Start
- Make sure R and all required packages are installed
- Check that the database file exists and is not corrupted
- Restart RStudio and try again

#### Data Not Saving
- Check your database connection status in the sidebar
- Make sure all required fields are filled out
- Look for error messages that may explain the issue

#### Incorrect Statistics
- Check that all game data was entered correctly
- Verify that players are assigned to the correct teams
- Make sure you're looking at the correct date range

### Getting Help
- Check the documentation in the `docs/` directory
- Look for error messages in the R console
- Contact the application administrator for assistance

## Conclusion

TRCStatR is designed to make baseball statistics accessible and educational for youth players and coaches. By following this guide, you can make the most of the application's features and help young players understand and improve their performance on the field.

Happy baseball!
