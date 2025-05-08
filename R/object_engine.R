# object_engine.R
# Loads all Level 1 (raw) objects (basic/directly collected stats from games, 
# Main Page logo, Team logos, Team colors, etc) for the TRCStatR app
# Creates all Level 2 objects (conventional stats that are calculated directly from basic stats:
# AB, ERA, WHIP, IP, etc.)
# Creates Level 3 Advanced stats (OPS+, WAR, wRC+, FIP, etc.), and calculates/stores league
# average stats that are needed to normalize and calculate those

# ================================================================================================
# SECTION 1: UTILITY FUNCTIONS
# ================================================================================================

#' Safely divide two numbers, handling division by zero
#'
#' @param numerator The numerator in the division operation
#' @param denominator The denominator in the division operation
#' @param default The default value to return if division is not possible (default: 0)
#' @param min_denominator The minimum value for the denominator to prevent near-zero division (default: 1e-10)
#' @return The result of the division or the default value
safe_divide <- function(numerator, denominator, default = 0, min_denominator = 1e-10) {
  # Vectorized safe division
  numerator <- as.numeric(numerator)
  denominator <- as.numeric(denominator)
  len <- max(length(numerator), length(denominator))
  if (length(numerator) != len) numerator <- rep(numerator, len)
  if (length(denominator) != len) denominator <- rep(denominator, len)
  invalid <- is.na(numerator) | is.na(denominator) | abs(denominator) < min_denominator
  result <- rep(default, len)
  result[!invalid] <- numerator[!invalid] / denominator[!invalid]
  return(result)
}

#' Safely calculate a batting average
#'
#' @param hits Number of hits
#' @param at_bats Number of at-bats
#' @param default Default value to return if calculation is not possible (default: 0)
#' @return The calculated batting average or the default value
safe_batting_avg <- function(hits, at_bats, default = 0) {
  avg <- safe_divide(hits, at_bats, default)
  # Ensure the result is between 0 and 1
  return(pmax(0, pmin(1, avg)))
}

#' Safely calculate on-base percentage
#'
#' @param hits Number of hits
#' @param walks Number of walks
#' @param hbp Number of hit by pitch
#' @param at_bats Number of at-bats
#' @param sf Number of sacrifice flies
#' @param default Default value to return if calculation is not possible (default: 0)
#' @return The calculated on-base percentage or the default value
safe_obp <- function(hits, walks, hbp, at_bats, sf, default = 0) {
  obp <- safe_divide(hits + walks + hbp, at_bats + walks + hbp + sf, default)
  # Ensure the result is between 0 and 1
  return(pmax(0, pmin(1, obp)))
}

#' Safely calculate slugging percentage
#'
#' @param total_bases Total bases (singles + 2*doubles + 3*triples + 4*home_runs)
#' @param at_bats Number of at-bats
#' @param default Default value to return if calculation is not possible (default: 0)
#' @return The calculated slugging percentage or the default value
safe_slg <- function(total_bases, at_bats, default = 0) {
  slg <- safe_divide(total_bases, at_bats, default)
  # Ensure the result is reasonable (technically SLG can be > 1)
  return(pmax(0, slg))
}

#' Safely calculate ERA (Earned Run Average)
#'
#' @param earned_runs Number of earned runs
#' @param innings_pitched Number of innings pitched
#' @param default Default value to return if calculation is not possible (default: 9.99)
#' @param max_era Maximum ERA to return (default: 99.99)
#' @return The calculated ERA or the default value
safe_era <- function(earned_runs, innings_pitched, default = 0.00, max_era = 999.99) {
  # Make sure inputs are treated as vectors
  earned_runs <- as.numeric(earned_runs)
  innings_pitched <- as.numeric(innings_pitched)
  
  # Vectorized division
  era <- safe_divide(9 * earned_runs, innings_pitched, default)
  
  # Use pmin for vectorized comparison
  return(pmin(era, rep(max_era, length(era))))
}

#' Safely calculate WHIP (Walks and Hits per Inning Pitched)
#'
#' @param walks Number of walks
#' @param hits Number of hits
#' @param innings_pitched Number of innings pitched
#' @param default Default value to return if calculation is not possible (default: 2.00)
#' @param max_whip Maximum WHIP to return (default: 9.99)
#' @return The calculated WHIP or the default value
safe_whip <- function(walks, hits, batters_hit, innings_pitched, default = 0.000, max_whip = 99.999) {
  whip <- safe_divide(walks + hits + batters_hit, innings_pitched, default)
  # Cap extremely high WHIPs
  return(pmin(whip, max_whip))
}

#' Safely calculate a rate statistic per 9 innings
#'
#' @param value The value to calculate the rate for (e.g., strikeouts, walks)
#' @param innings_pitched Number of innings pitched
#' @param default Default value to return if calculation is not possible (default: 0)
#' @return The calculated rate per 9 innings or the default value
safe_per_nine <- function(value, innings_pitched, default = 0) {
  return(safe_divide(9 * value, innings_pitched, default))
}

#' Fixed K/BB ratio calculation
#' Handles edge cases: returns "Inf" when BB=0 and K>0, returns 0 when K=0
#'
#' @param k Number of strikeouts
#' @param bb Number of walks
#' @return The calculated K/BB ratio, "Inf" when bb=0 and k>0, or 0 when k=0
safe_k_bb_ratio <- function(k, bb) {
  # Make sure inputs are numeric
  k <- as.numeric(k)
  bb <- as.numeric(bb)
  
  # Handle cases
  if (bb == 0) {
    if (k > 0) {
      return(Inf)  # Infinite ratio when no walks but some strikeouts
    } else {
      return(0)    # No strikeouts, no walks -> 0 ratio
    }
  } else if (k == 0) {
    return(0)      # No strikeouts, some walks -> 0 ratio
  } else {
    return(k / bb) # Normal case
  }
}

#' Improved OPS+ calculation handling
#'
#' @param player_ops Player's OPS (on-base percentage + slugging percentage)
#' @param league_ops League average OPS
#' @param default Default value to return if calculation is not possible (default: 100)
#' @return The calculated OPS+ or the default value
safe_ops_plus <- function(player_ops, league_ops, default = 100) {
  # Handle NAs or invalid values
  if (is.na(player_ops) || is.na(league_ops) || league_ops <= 0) {
    return(default)
  }
  
  # Calculate OPS+ (100 * playerOPS / leagueOPS)
  ops_plus <- round(100 * player_ops / league_ops)
  
  return(ops_plus)
}

#' Improved ERA+ calculation that handles edge cases
#'
#' @param player_era Player's ERA
#' @param league_era League average ERA
#' @param default Default value to return if calculation is not possible (default: 100)
#' @return The calculated ERA+ or the default value
safe_era_plus <- function(player_era, league_era, default = 100) {
  # If either ERA is NA or 0, handle appropriately
  if (is.na(player_era) || is.na(league_era)) {
    return(default)
  }
  
  # Calculate ERA+ (100 * leagueERA / playerERA + small value)
  era_plus <- round(100 * league_era / (player_era + 0.25))
  return(era_plus)
}

#' Improved WHIP+ calculation
#'
#' @param player_whip Player's WHIP
#' @param league_whip League average WHIP
#' @param default Default value to return if calculation is not possible (default: 100)
#' @return The calculated WHIP+ or the default value
safe_whip_plus <- function(player_whip, league_whip, default = 100) {
  # Handle NAs or invalid values
  if (is.na(player_whip) || is.na(league_whip) || player_whip <= 0) {
    return(default)
  }
  
  # Calculate WHIP+ (100 * leagueWHIP / playerWHIP + small value)
  whip_plus <- round(100 * league_whip / (player_whip + 0.01))
  
  return(whip_plus)
}

#' Safely calculate a scaling factor
#'
#' @param total The total value (e.g., season length)
#' @param parts The parts value (e.g., games played)
#' @param default Default value to return if calculation is not possible (default: 1)
#' @return The calculated scaling factor or the default value
safe_scale_factor <- function(total, parts, default = 1) {
  return(safe_divide(total, parts, default))
}

# ================================================================================================
# SECTION 2: DATA LOADING
# ================================================================================================

# ---- DATA TABLES ----
players                       <- readRDS("tblPlayers.rds")
teams                         <- readRDS("tblTeams.rds")
games                         <- readRDS("tblGames.rds")
batting                       <- readRDS("tblBattingStatsFlat.rds")
pitching                      <- readRDS("tblPitchingStatsFlat.rds")
divisions                     <- readRDS("tblDivisions.rds")
autosave_raw                  <- readRDS("game_entry_autosave.rds")

# ---- THEME/CSS ASSETS ----
main_css        <- "www/styles.css"

# ================================================================================================
# SECTION 3: HELPER FUNCTIONS
# ================================================================================================

# ---- IMAGE ASSETS ----

# Helper: Get logo path for a given team name
get_team_logo <- function(team_name) {
  # Normalize team name for variable lookup
  clean_name <- gsub(" ", "_", tolower(team_name))
  logo_var <- paste0(clean_name, "_logo")
  if (exists(logo_var, inherits = FALSE)) {
    return(get(logo_var, inherits = FALSE))
  }
  # Fallback: construct path as in variable convention
  return(paste0("www/", clean_name, ".png"))
}

team_colors <- list(
  Cubs = list(primary = "#0E3386", secondary = "#CC3433", tertiary = "#FFFFFF"),
  Giants = list(primary = "#FD5A1E", secondary = "#27251F", tertiary = "#AE8F6F"),
  Guardians = list(primary = "#00385D", secondary = "#E50022", tertiary = "#FFFFFF"),
  Mets = list(primary = "#002D72", secondary = "#FF5910", tertiary = "#FFFFFF"),
  Orioles = list(primary = "#DF4601", secondary = "#000000", tertiary = "#FFFFFF"),
  "Red Sox" = list(primary = "#BD3039", secondary = "#0C2340", tertiary = "#FFFFFF"),
  Royals = list(primary = "#174885", secondary = "#7BB2DD", tertiary = "#C0995A"),
  Tigers = list(primary = "#182D55", secondary = "#F26722", tertiary = "#C4CED4")
)

trc_logo <- "www/tiger-logo.png"
cubs_logo <- "www/cubs.png"
giants_logo <- "www/giants.png"
guardians_logo <- "www/guardians.png"
mets_logo <- "www/mets.png"
orioles_logo <- "www/orioles.png"
red_sox_logo <- "www/red_sox.png"
royals_logo <- "www/royals.png"
tigers_logo <- "www/tigers.png"

# ================================================================================================
# SECTION 4: LEVEL 1 STATS (RAW DATA)
# ================================================================================================

# Get basic player and game info
player_id <- players$PlayerID
first_name <- players$FirstName
last_initial <- players$LastInitial
jersey_number <- players$JerseyNumber
team_id <- players$TeamID
division <- divisions$DivisionName
team_name <- teams$TeamName
game_id <- games$GameID
game_date <- games$GameDate
home_team_id <- games$HomeTeamID
away_team_id <- games$AwayTeamID
home_team_score <- games$HomeScore
away_team_score <- games$AwayScore
game_completed <- games$IsComplete

# Aggregate batting stats by player (sum across all games)
# First, get unique player IDs from batting data
batting_player_ids <- unique(batting$PlayerID)

# Initialize vectors to store aggregated stats
agg_plate_app <- numeric(length(batting_player_ids))
agg_singles <- numeric(length(batting_player_ids))
agg_doubles <- numeric(length(batting_player_ids))
agg_triples <- numeric(length(batting_player_ids))
agg_home_runs <- numeric(length(batting_player_ids))
agg_walks <- numeric(length(batting_player_ids))
agg_hit_by_pitch <- numeric(length(batting_player_ids))
agg_sac_flies <- numeric(length(batting_player_ids))
agg_sac_hits <- numeric(length(batting_player_ids))
agg_strikeouts <- numeric(length(batting_player_ids))
agg_grounded_into_dp <- numeric(length(batting_player_ids))
agg_steals <- numeric(length(batting_player_ids))
agg_caught_stealing <- numeric(length(batting_player_ids))
agg_reached_on_error <- numeric(length(batting_player_ids))
agg_rbis <- numeric(length(batting_player_ids))

# Aggregate stats for each player
for (i in seq_along(batting_player_ids)) {
  player_rows <- which(batting$PlayerID == batting_player_ids[i])
  
  agg_plate_app[i] <- sum(batting$PA[player_rows], na.rm = TRUE)
  agg_singles[i] <- sum(batting$X1B[player_rows], na.rm = TRUE)
  agg_doubles[i] <- sum(batting$X2B[player_rows], na.rm = TRUE)
  agg_triples[i] <- sum(batting$X3B[player_rows], na.rm = TRUE)
  agg_home_runs[i] <- sum(batting$HR[player_rows], na.rm = TRUE)
  agg_walks[i] <- sum(batting$BB[player_rows], na.rm = TRUE)
  agg_hit_by_pitch[i] <- sum(batting$HBP[player_rows], na.rm = TRUE)
  agg_sac_flies[i] <- sum(batting$SF[player_rows], na.rm = TRUE)
  agg_sac_hits[i] <- sum(batting$SH[player_rows], na.rm = TRUE)
  agg_strikeouts[i] <- sum(batting$K[player_rows], na.rm = TRUE)
  agg_grounded_into_dp[i] <- sum(batting$GIDP[player_rows], na.rm = TRUE)
  agg_steals[i] <- sum(batting$SB[player_rows], na.rm = TRUE)
  agg_caught_stealing[i] <- sum(batting$CS[player_rows], na.rm = TRUE)
  agg_reached_on_error[i] <- sum(batting$ROE[player_rows], na.rm = TRUE)
  agg_rbis[i] <- sum(batting$RBI[player_rows], na.rm = TRUE)
}

# Aggregate pitching stats by player (sum across all games)
# First, get unique player IDs from pitching data
pitching_player_ids <- unique(pitching$PlayerID)

# Initialize vectors to store aggregated stats
agg_outs_recorded <- numeric(length(pitching_player_ids))
agg_batters_faced <- numeric(length(pitching_player_ids))
agg_hits_allowed <- numeric(length(pitching_player_ids))
agg_runs_allowed <- numeric(length(pitching_player_ids))
agg_earned_runs <- numeric(length(pitching_player_ids))
agg_walks_allowed <- numeric(length(pitching_player_ids))
agg_batters_struck_out <- numeric(length(pitching_player_ids))
agg_hit_batsmen <- numeric(length(pitching_player_ids))
agg_hr_allowed <- numeric(length(pitching_player_ids))

# Aggregate stats for each player
for (i in seq_along(pitching_player_ids)) {
  player_rows <- which(pitching$PlayerID == pitching_player_ids[i])
  
  agg_outs_recorded[i] <- sum(pitching$OutsRecorded[player_rows], na.rm = TRUE)
  agg_batters_faced[i] <- sum(pitching$BF[player_rows], na.rm = TRUE)
  agg_hits_allowed[i] <- sum(pitching$H[player_rows], na.rm = TRUE)
  agg_runs_allowed[i] <- sum(pitching$R[player_rows], na.rm = TRUE)
  agg_earned_runs[i] <- sum(pitching$ER[player_rows], na.rm = TRUE)
  agg_walks_allowed[i] <- sum(pitching$BB[player_rows], na.rm = TRUE)
  agg_batters_struck_out[i] <- sum(pitching$K[player_rows], na.rm = TRUE)
  agg_hit_batsmen[i] <- sum(pitching$HBP[player_rows], na.rm = TRUE)
  agg_hr_allowed[i] <- sum(pitching$HR[player_rows], na.rm = TRUE)
}

# Use the aggregated vectors for all subsequent calculations
plate_app <- agg_plate_app
singles <- agg_singles
doubles <- agg_doubles
triples <- agg_triples
home_runs <- agg_home_runs
walks <- agg_walks
hit_by_pitch <- agg_hit_by_pitch
sac_flies <- agg_sac_flies
sac_hits <- agg_sac_hits
strikeouts <- agg_strikeouts
grounded_into_dp <- agg_grounded_into_dp
steals <- agg_steals
caught_stealing <- agg_caught_stealing
reached_on_error <- agg_reached_on_error
rbis <- agg_rbis

# Use aggregated pitching stats
outs_recorded <- agg_outs_recorded
batters_faced <- agg_batters_faced
hits_allowed <- agg_hits_allowed
runs_allowed <- agg_runs_allowed
earned_runs <- agg_earned_runs
walks_allowed <- agg_walks_allowed
batters_struck_out <- agg_batters_struck_out
batters_hit <- agg_hit_batsmen
hr_allowed <- agg_hr_allowed

# Store the player IDs for reference
batting_ids <- batting_player_ids
pitching_ids <- pitching_player_ids

# Make sure we're using the correct data sources for all players
# No special cases - all players should be handled the same way

# Expose aggregated vectors to global environment for league leaders module
# These assignments have been moved to the Global Assignments section

# ================================================================================================
# SECTION 5: LEVEL 2 STATS (DERIVED STATS)
# ================================================================================================

# Batting Level 2 Stats
AB <- plate_app - walks - hit_by_pitch - sac_flies - sac_hits
H <- singles + doubles + triples + home_runs
TB <- singles + (2 * doubles) + (3 * triples) + (4 * home_runs)
AVG <- safe_batting_avg(H, AB)
OBP <- safe_obp(H, walks, hit_by_pitch, AB, sac_flies)
SLG <- safe_slg(TB, AB)
OPS <- OBP + SLG
XBH <- doubles + triples + home_runs
BB_rate <- safe_divide(walks, plate_app)
K_rate <- safe_divide(strikeouts, plate_app)
SB_rate <- safe_divide(steals, steals + caught_stealing)
GIDP_rate <- safe_divide(grounded_into_dp, plate_app)
RBI_rate <- safe_divide(rbis, plate_app)
ROE_rate <- safe_divide(reached_on_error, plate_app)
ISO <- SLG - AVG
BABIP <- safe_divide(H - home_runs, AB - strikeouts - home_runs + sac_flies)

# These assignments have been moved to the Global Assignments section

# Pitching Level 2 Stats
IP_standalone <- (outs_recorded %/% 3) + (outs_recorded %% 3) / 10
IP <- outs_recorded / 3
ERA <- safe_era(earned_runs, IP)
# Updated WHIP calculation to include hit batsmen (HBP)
WHIP <- safe_whip(walks_allowed, hits_allowed, batters_hit, IP)
K_per_9 <- safe_per_nine(batters_struck_out, IP)
BB_per_9 <- safe_per_nine(walks_allowed, IP)
HR_per_9 <- safe_per_nine(hr_allowed, IP)
HBP_per_9 <- safe_per_nine(batters_hit, IP)
K_pct <- safe_divide(batters_struck_out, batters_faced)
BB_pct <- safe_divide(walks_allowed, batters_faced)

# Updated K/BB ratio calculation using new safe function
K_BB_ratio <- sapply(seq_along(batters_struck_out), function(i) {
  safe_k_bb_ratio(batters_struck_out[i], walks_allowed[i])
})

# These assignments have been moved to the Global Assignments section

# Team/General Level 2 Stats (aggregated at team level)
# Aggregate total runs (RBI), games played, runs scored, and runs allowed per team
team_ids <- unique(players$TeamID)
total_runs <- sapply(team_ids, function(tid) sum(rbis[players$TeamID == tid], na.rm = TRUE))
games_played <- sapply(team_ids, function(tid) {
  # Count unique games for each team (as home or away)
  length(unique(games$GameID[games$HomeTeamID == tid | games$AwayTeamID == tid]))
})
# Aggregate runs scored and allowed per team from the games data
runs_scored <- sapply(team_ids, function(tid) {
  sum(games$HomeScore[games$HomeTeamID == tid], na.rm = TRUE) +
    sum(games$AwayScore[games$AwayTeamID == tid], na.rm = TRUE)
})
runs_allowed <- sapply(team_ids, function(tid) {
  sum(games$AwayScore[games$HomeTeamID == tid], na.rm = TRUE) +
    sum(games$HomeScore[games$AwayTeamID == tid], na.rm = TRUE)
})
runs_per_game <- safe_divide(total_runs, games_played) # (aggregate at team level)
# Optionally, also aggregate wins/losses and run_diff at team level if needed
# win_pct and run_diff can be similarly defined if those variables are used elsewhere

pythag_pct <- safe_divide(runs_scored^2, runs_scored^2 + runs_allowed^2) # (team level)

# ================================================================================================
# SECTION 6: LEVEL 3 STATS (ADVANCED METRICS)
# ================================================================================================

# ---------- CALCULATE LEAGUE AVERAGES --------------------------------
SeasonID <- "1"
DivisionID <- "1"

# Basic league averages with safe calculations
League_BA <- round(safe_divide(sum(H, na.rm = TRUE), sum(AB, na.rm = TRUE)), 3)
League_OBP <- round(safe_obp(sum(H, na.rm = TRUE), 
                             sum(walks, na.rm = TRUE), 
                             sum(hit_by_pitch, na.rm = TRUE), 
                             sum(AB, na.rm = TRUE), 
                             sum(sac_flies, na.rm = TRUE)), 3)
League_SLG <- round(safe_slg(sum(TB, na.rm = TRUE), sum(AB, na.rm = TRUE)), 3)
League_OPS <- round(League_OBP + League_SLG, 3)
League_BABIP <- round(safe_divide(sum(H - home_runs, na.rm = TRUE), 
                                  sum(AB - strikeouts - home_runs + sac_flies, na.rm = TRUE)), 3)
League_ISO <- League_SLG - League_BA
League_BB_Percent <- safe_divide(sum(walks, na.rm = TRUE), sum(plate_app, na.rm = TRUE))
League_K_Percent <- safe_divide(sum(strikeouts, na.rm = TRUE), sum(plate_app, na.rm = TRUE))
League_SB_Percent <- safe_divide(sum(steals, na.rm = TRUE), 
                                 sum(steals + caught_stealing, na.rm = TRUE))

# Pitching league averages
League_ERA <- round(safe_era(sum(earned_runs, na.rm = TRUE), sum(IP, na.rm = TRUE)), 2)
# Updated League WHIP calculation to include hit batsmen (HBP)
League_WHIP <- round(safe_whip(sum(walks_allowed, na.rm = TRUE), 
                               sum(hits_allowed, na.rm = TRUE), 
                               sum(batters_hit, na.rm = TRUE),
                               sum(IP, na.rm = TRUE)), 2)

League_K9 <- safe_per_nine(sum(batters_struck_out, na.rm = TRUE), sum(IP, na.rm = TRUE))
League_BB9 <- safe_per_nine(sum(walks_allowed, na.rm = TRUE), sum(IP, na.rm = TRUE))
League_HR9 <- safe_per_nine(sum(hr_allowed, na.rm = TRUE), sum(IP, na.rm = TRUE))
League_KBB <- safe_divide(sum(batters_struck_out, na.rm = TRUE), 
                          sum(walks_allowed, na.rm = TRUE))

# FIP Calculation with safe division
# For youth baseball, we'll adjust the FIP formula to give more weight to strikeouts
# This better reflects the value of dominant pitchers at this level
FIP_numerator <- (13 * hr_allowed) + (3 * (walks_allowed + batters_hit)) - (3 * batters_struck_out) 
# modified from 3 * batters_struck_out because there is an exorbitant amount of strikeouts thus far
FIP <- safe_divide(FIP_numerator, IP, default = 4.20) # 4.20 is roughly league average FIP

# League FIP Calculation with safe division
# Use the same formula as player FIP for consistency
League_FIP_numerator <- (13 * sum(hr_allowed, na.rm = TRUE)) + 
  (3 * sum(walks_allowed, na.rm = TRUE) + sum(batters_hit, na.rm = TRUE)) - 
  (2 * sum(batters_struck_out, na.rm = TRUE))
#modified from 3 * batters_struck_out because there is an exorbitant amount of strikeouts thus far
League_FIP <- safe_divide(League_FIP_numerator, sum(IP, na.rm = TRUE), default = 4.20)

# FIP constant calculation
# This equalizes FIP to ERA for easier comparison
# For youth baseball, we'll use a much smaller constant to prevent inflated FIPs
if (sum(IP, na.rm = TRUE) > 1) { # Ensure we have enough IP to be meaningful
  # Cap the constant at a reasonable value for youth baseball
  FIP_constant <- abs(min(FIP))
}
# Apply FIP constant to individual FIP values
FIP <- FIP + FIP_constant

# Create a named vector of FIP values for direct lookup
FIP_named <- setNames(FIP, as.character(pitching_ids))

# Print some key FIP values for debugging

for (player_name in c("Gabriel S", "Ethan F", "Garrett J")) {
  player_idx <- which(paste(players$FirstName, players$LastInitial) == player_name)
  if (length(player_idx) > 0) {
    player_id <- players$PlayerID[player_idx[1]]
    player_id_char <- as.character(player_id)
    if (player_id_char %in% names(FIP_named)) {

    } else {

    }
  }
}

# Store the named FIP vector in the global environment
assign("FIP_named", FIP_named, envir = .GlobalEnv)

# ---- PLAYER NORMALIZED STATS ----

# OPS+ (normalized OPS, 100 is league average, > 100 is above average)
# Updated to use safe_ops_plus with better handling of edge cases
OPS_plus <- sapply(seq_along(OPS), function(i) {
  if (is.na(OPS[i]) || OPS[i] <= 0) {
    return(100)  # Default to league average for invalid values
  }
  round(100 * OPS[i] / League_OPS)
})

# ERA+ (normalized ERA, 100 is league average, > 100 is above average)
# Updated to use safe_era_plus
ERA_plus <- sapply(seq_along(ERA), function(i) safe_era_plus(ERA[i], League_ERA))

# WHIP+ (normalized WHIP, 100 is league average, > 100 is better)
# Note: For WHIP, lower is better, so we invert the calculation
# Updated to use safe_whip_plus
WHIP_plus <- sapply(seq_along(WHIP), function(i) safe_whip_plus(WHIP[i], League_WHIP))

# ---- ADVANCED METRICS ----

# wOBA Weights - based on run expectancy models
wOBA_weights <- list(
  wBB = 0.69,
  wHBP = 0.72,
  w1B = 0.89,
  w2B = 1.27,
  w3B = 1.62,
  wHR = 2.10
)

# Calculate wOBA (Weighted On-Base Average)
wOBA_numerator <- (wOBA_weights$wBB * walks) + 
  (wOBA_weights$wHBP * hit_by_pitch) + 
  (wOBA_weights$w1B * singles) + 
  (wOBA_weights$w2B * doubles) + 
  (wOBA_weights$w3B * triples) + 
  (wOBA_weights$wHR * home_runs)
wOBA_denominator <- plate_app # Or more precisely AB + BB + SF + HBP
wOBA <- safe_divide(wOBA_numerator, wOBA_denominator)

# Calculate League wOBA
League_wOBA_numerator <- (wOBA_weights$wBB * sum(walks, na.rm = TRUE)) + 
  (wOBA_weights$wHBP * sum(hit_by_pitch, na.rm = TRUE)) + 
  (wOBA_weights$w1B * sum(singles, na.rm = TRUE)) + 
  (wOBA_weights$w2B * sum(doubles, na.rm = TRUE)) + 
  (wOBA_weights$w3B * sum(triples, na.rm = TRUE)) + 
  (wOBA_weights$wHR * sum(home_runs, na.rm = TRUE))
League_wOBA_denominator <- sum(plate_app, na.rm = TRUE)

# For youth baseball, we'll use a lower baseline for League wOBA
# This ensures that good performances aren't unfairly penalized
raw_league_wOBA <- safe_divide(League_wOBA_numerator, League_wOBA_denominator, default = 0.320)

# Use a more appropriate baseline for youth baseball (around .330 is typical for MLB)
League_wOBA <- min(raw_league_wOBA, 0.400)

# wOBA Scale (to convert wOBA to runs)
League_wOBA_Scale <- safe_divide(League_OBP, League_wOBA, default = 1.15)
League_wOBA <- League_wOBA * League_wOBA_Scale

# wRAA (Weighted Runs Above Average)
# Simplified calculation that properly handles players with high wOBA
wRAA <- safe_divide((wOBA - League_wOBA), League_wOBA_Scale) * plate_app

# wBC (Weighted Bases Created) - Gus Murphy's custom metric
# This is a specialized metric for youth baseball that measures total offensive contribution
# Formula: (1B + 2B*1.5 + 3B*2 + HR*2.5 + BB*0.75 + HBP*0.75 + SB*0.25 - CS*0.5 - (K*0.25)) / PA

# Calculate the numerator first
wBC_numerator <- (singles * 1.25) + (doubles * 2.5) + (triples * 3.75) + (home_runs * 5) + 
  (walks * 0.75) + (hit_by_pitch * 0.75) + (steals * 0.5) - 
  (caught_stealing * 0.5) - (strikeouts * 0.5)

# Special case: if numerator is 0 and denominator > 0, result should be 0 not default
wBC <- numeric(length(plate_app))
for (i in 1:length(plate_app)) {
  if (wBC_numerator[i] == 0 && plate_app[i] > 0) {
    # Player had PAs but did nothing productive, should be 0
    wBC[i] <- 0
  } else {
    # Normal case or no PAs
    wBC[i] <- safe_divide(wBC_numerator[i], plate_app[i], default = 0)
  }
}

# sbWAR (Simplified Batting WAR - MLB season equivalent)
# Scale to 600 PA (typical full MLB season), but with a cap for small sample sizes
# For youth baseball, we need to be more conservative with small samples
scale_factor <- pmin(safe_scale_factor(600, plate_app, default = 1), 10) # Cap scaling at 10x

# For youth baseball with extreme performers, we need to ensure we're properly rewarding high performance
# We'll use a more generous approach for players with very high wOBA
wRAA_adjusted <- wRAA
# Boost wRAA for exceptional performers (wOBA > 0.500)
exceptional_idx <- which(wOBA > 0.500)
if (length(exceptional_idx) > 0) {
  wRAA_adjusted[exceptional_idx] <- wRAA[exceptional_idx] * 1.5  # 50% boost for exceptional performers
}

# Calculate raw sbWAR before adjustment
raw_sbWAR <- (scale_factor * wRAA_adjusted) / 10

# Add adjustment constant to center league average at 0.0
# This adjustment is critical for ensuring consistent WAR values
sbWAR <- raw_sbWAR + 0.3255368 # Adjustment to center at 0

# spWAR (Simplified Pitching WAR - MLB season equivalent)
# Calculate Runs Prevented Above Average
RPAA <- safe_divide((League_ERA - ERA) * IP, 9)

# For youth baseball with extreme performers like Gabriel, we need a more reasonable scaling approach
# First, apply a logarithmic scaling to compress extreme values
RPAA_scaled <- RPAA
if (any(RPAA > 3)) {
  # Apply logarithmic scaling to values above 3 to compress the range
  high_idx <- which(RPAA > 3)
  RPAA_scaled[high_idx] <- 3 + log10(RPAA[high_idx] - 2)  # Compress values above 3
}

# Identify exceptional pitchers (ERA < 1.00 with significant IP)
exceptional_idx <- which(ERA < 1.00 & IP >= 3)
if (length(exceptional_idx) > 0) {
  # Apply a more modest boost for exceptional performers
  RPAA_scaled[exceptional_idx] <- RPAA_scaled[exceptional_idx] * 1.25  # 25% boost instead of 50%
}

# Apply a scaling factor to bring values into a more reasonable range (3-5 for exceptional performers)
scaling_factor <- 0.6  # Reduce the overall scale

# Scale to MLB season (180-200 IP for a starter) but avoid double-dividing by 9
# Add adjustment constant to center league average at 0.0 for pitchers
spWAR <- (RPAA_scaled * scaling_factor) + ( -0.02567139 ) # Reduced scaling + adjustment constant
# Apply small penalty for non-pitchers
# Create named vector for spWAR
spWAR_vec <- setNames(spWAR, pitching_ids)
# Define all_ids here to ensure it's available before use
if (!exists("all_ids", envir = .GlobalEnv)) {
  all_ids <- players$PlayerID
  assign("all_ids", all_ids, envir = .GlobalEnv)
}

# Align OPS+ to all players for consistent indexing
OPS_plus_vec <- setNames(OPS_plus, batting_ids)
OPS_plus_aligned <- OPS_plus_vec[as.character(all_ids)]
OPS_plus_aligned[is.na(OPS_plus_aligned)] <- 100  # Set to league average for non-batters

# Align to all players, filling missing with -0.1 (small penalty for non-pitchers)
spWAR_aligned <- spWAR_vec[as.character(all_ids)]
spWAR_aligned[is.na(spWAR_aligned)] <- -0.1 # Small fixed penalty for non-pitchers

# gWAR (Gus WAR) - custom WAR metric - added constant for now to make league average gWAR = 0
# For extreme performers, boost the wBC component
wBC_adjusted <- wBC
# Identify exceptional offensive performers
exceptional_idx <- which(wBC > 0.5)
if (length(exceptional_idx) > 0) {
  wBC_adjusted[exceptional_idx] <- wBC[exceptional_idx] * 1.3  # 30% boost for exceptional performers
}
gWAR <- safe_divide((wBC_adjusted * plate_app), 10) + ( -0.3834211 )

# Weighted Total WAR (wTWAR) - combined metric
# Weights batting (40%), pitching (30%), and Gus's formula (30%)
# all_ids was already defined and assigned to global environment above

# Create named vectors for each stat using the aggregated player IDs
spWAR_vec <- setNames(spWAR, pitching_ids)
sbWAR_vec <- setNames(sbWAR, batting_ids)
gWAR_vec  <- setNames(gWAR, batting_ids)

# Align to all players, filling missing with -0.1 (small penalty for non-pitchers)
spWAR_aligned <- spWAR_vec[as.character(all_ids)]
spWAR_aligned[is.na(spWAR_aligned)] <- -0.1 # Small fixed penalty for non-pitchers
sbWAR_aligned <- sbWAR_vec[as.character(all_ids)]; sbWAR_aligned[is.na(sbWAR_aligned)] <- 0
gWAR_aligned  <- gWAR_vec[as.character(all_ids)];  gWAR_aligned[is.na(gWAR_aligned)]  <- 0

# Analyze WAR values to ensure means are around 0
# Log the original means for debugging
log_message <- paste("Original WAR means - sbWAR:", round(mean(sbWAR_aligned, na.rm = TRUE), 4),
                    "spWAR:", round(mean(spWAR_aligned, na.rm = TRUE), 4),
                    "gWAR:", round(mean(gWAR_aligned, na.rm = TRUE), 4))
print(log_message)

# Calculate means for each WAR component
sbWAR_mean <- mean(sbWAR_aligned, na.rm = TRUE)
spWAR_mean <- mean(spWAR_aligned, na.rm = TRUE)
gWAR_mean <- mean(gWAR_aligned, na.rm = TRUE)

# Apply adjustments to center means at 0
# These adjustments ensure that the league average for each WAR component is 0
sbWAR_adjustment <- -sbWAR_mean
spWAR_adjustment <- -spWAR_mean
gWAR_adjustment <- -gWAR_mean

# Log the adjustments for debugging
log_message <- paste("WAR adjustments - sbWAR:", round(sbWAR_adjustment, 4),
                    "spWAR:", round(spWAR_adjustment, 4),
                    "gWAR:", round(gWAR_adjustment, 4))
print(log_message)

# Apply the adjustments
sbWAR_aligned <- sbWAR_aligned + sbWAR_adjustment
spWAR_aligned <- spWAR_aligned + spWAR_adjustment
gWAR_aligned <- gWAR_aligned + gWAR_adjustment

# Verify the adjustments worked
# Log the adjusted means for debugging
log_message <- paste("Adjusted WAR means - sbWAR:", round(mean(sbWAR_aligned, na.rm = TRUE), 4),
                    "spWAR:", round(mean(spWAR_aligned, na.rm = TRUE), 4),
                    "gWAR:", round(mean(gWAR_aligned, na.rm = TRUE), 4))
print(log_message)

# Calculate ranges and identify outliers
# Log min/max values for each WAR component
log_message <- paste("WAR ranges - sbWAR:", round(min(sbWAR_aligned, na.rm = TRUE), 2), "to", round(max(sbWAR_aligned, na.rm = TRUE), 2),
                    "spWAR:", round(min(spWAR_aligned, na.rm = TRUE), 2), "to", round(max(spWAR_aligned, na.rm = TRUE), 2),
                    "gWAR:", round(min(gWAR_aligned, na.rm = TRUE), 2), "to", round(max(gWAR_aligned, na.rm = TRUE), 2))
print(log_message)

# Now calculate wTWAR with the adjusted values
# Using the 40/30/30 weighting (40% batting, 30% pitching, 30% fielding/other)
wTWAR <- (0.4 * sbWAR_aligned) + (0.3 * spWAR_aligned) + (0.3 * gWAR_aligned)

# All global assignments have been moved to the Global Assignments section

# ================================================================================================
# SECTION 7: GLOBAL ASSIGNMENTS
# ================================================================================================

# Centralized WAR accessor function - single source of truth for all WAR values
# This ensures all modules display consistent WAR values
get_player_war <- function(player_id) {
  # Convert player_id to character for lookup
  player_id_char <- as.character(player_id)
  
  # Initialize default values
  result <- list(
    sbWAR = 0,
    spWAR = -0.1,  # Default non-pitcher penalty
    gWAR = 0,
    wTWAR = 0
  )
  
  # Get the player's index in the aligned vectors
  player_idx <- which(as.character(all_ids) == player_id_char)
  
  if (length(player_idx) > 0) {
    # Player found, retrieve WAR values
    result$sbWAR <- sbWAR_aligned[player_idx]
    result$spWAR <- spWAR_aligned[player_idx]
    result$gWAR <- gWAR_aligned[player_idx]
    
    # Get player's basic stats to validate WAR values
    player_stats <- NULL
    if (exists("batting", envir = .GlobalEnv)) {
      batting_idx <- which(batting$PlayerID == player_id)
      if (length(batting_idx) > 0) {
        # Calculate basic offensive metrics
        pa <- sum(batting$PA[batting_idx], na.rm = TRUE)
        h <- sum(batting$X1B[batting_idx] + batting$X2B[batting_idx] + 
               batting$X3B[batting_idx] + batting$HR[batting_idx], na.rm = TRUE)
        hr <- sum(batting$HR[batting_idx], na.rm = TRUE)
        bb <- sum(batting$BB[batting_idx], na.rm = TRUE)
        
        # Log player stats for debugging
        player_name <- ""
        if (exists("players", envir = .GlobalEnv)) {
          player_name_idx <- which(players$PlayerID == player_id)
          if (length(player_name_idx) > 0) {
            player_name <- paste(players$FirstName[player_name_idx[1]], players$LastInitial[player_name_idx[1]])
          }
        }
        
        # Special handling for Everett Z if that's who we're looking at
        if (grepl("Everett Z", player_name)) {
          print(paste("Checking Everett Z's stats - PA:", pa, "H:", h, "HR:", hr, "BB:", bb, "sbWAR:", round(result$sbWAR, 2)))
        }
        
        # If player has good offensive stats but negative sbWAR, adjust it
        if (pa >= 50 && (h/pa) > 0.300 && result$sbWAR < 0) {
          # Adjust sbWAR to be at least slightly positive for good hitters
          result$sbWAR <- max(0.1, result$sbWAR)
          print(paste("Adjusted sbWAR for good hitter", player_name, "from negative to", round(result$sbWAR, 2)))
        }
        
        # If player has power but negative gWAR, adjust it
        if (hr >= 5 && result$gWAR < 0) {
          # Adjust gWAR to be at least neutral for power hitters
          result$gWAR <- max(0, result$gWAR)
          print(paste("Adjusted gWAR for power hitter", player_name, "from negative to", round(result$gWAR, 2)))
        }
      }
    }
    
    # Check if player is a pitcher with good stats but negative spWAR
    if (exists("pitching", envir = .GlobalEnv)) {
      pitching_idx <- which(pitching$PlayerID == player_id)
      if (length(pitching_idx) > 0) {
        ip <- sum(pitching$OutsRecorded[pitching_idx], na.rm = TRUE) / 3
        k <- sum(pitching$K[pitching_idx], na.rm = TRUE)
        er <- sum(pitching$ER[pitching_idx], na.rm = TRUE)
        
        # If pitcher has good stats but negative spWAR, adjust it
        if (ip >= 10 && (k/ip) >= 0.8 && (er/ip) <= 3.5 && result$spWAR < 0) {
          # Adjust spWAR to be at least slightly positive for good pitchers
          result$spWAR <- max(0.1, result$spWAR)
        }
      }
    }
    
    # Calculate wTWAR with the 40/30/30 weighting
    result$wTWAR <- (0.4 * result$sbWAR) + (0.3 * result$spWAR) + (0.3 * result$gWAR)
  }
  
  return(result)
}

# Add data accessor functions for league standings
get_games <- function() {
  if (exists("games", envir = .GlobalEnv)) {
    games <- get("games", envir = .GlobalEnv)
    if (!is.null(games) && nrow(games) > 0) {
      return(games)
    }
  }

  return(NULL)
}

get_teams <- function() {
  if (exists("teams", envir = .GlobalEnv)) {
    teams <- get("teams", envir = .GlobalEnv)
    if (!is.null(teams) && nrow(teams) > 0) {
      return(teams)
    }
  }

  return(NULL)
}

# Assign all Level 1 stats to global environment
assign("batting_player_ids", batting_player_ids, envir = .GlobalEnv)
assign("pitching_player_ids", pitching_player_ids, envir = .GlobalEnv)
assign("plate_app", plate_app, envir = .GlobalEnv)
assign("singles", singles, envir = .GlobalEnv)
assign("doubles", doubles, envir = .GlobalEnv)
assign("triples", triples, envir = .GlobalEnv)
assign("home_runs", home_runs, envir = .GlobalEnv)
assign("walks", walks, envir = .GlobalEnv)
assign("hit_by_pitch", hit_by_pitch, envir = .GlobalEnv)
assign("sac_flies", sac_flies, envir = .GlobalEnv)
assign("sac_hits", sac_hits, envir = .GlobalEnv)
assign("strikeouts", strikeouts, envir = .GlobalEnv)
assign("steals", steals, envir = .GlobalEnv)
assign("caught_stealing", caught_stealing, envir = .GlobalEnv)
assign("reached_on_error", reached_on_error, envir = .GlobalEnv)
assign("rbis", rbis, envir = .GlobalEnv)
assign("outs_recorded", outs_recorded, envir = .GlobalEnv)
assign("batters_faced", batters_faced, envir = .GlobalEnv)
assign("hits_allowed", hits_allowed, envir = .GlobalEnv)
assign("runs_allowed", runs_allowed, envir = .GlobalEnv)
assign("earned_runs", earned_runs, envir = .GlobalEnv)
assign("walks_allowed", walks_allowed, envir = .GlobalEnv)
assign("batters_struck_out", batters_struck_out, envir = .GlobalEnv)
assign("batters_hit", batters_hit, envir = .GlobalEnv)
assign("hr_allowed", hr_allowed, envir = .GlobalEnv)

# Assign Level 2 batting stats to global environment
assign("AB", AB, envir = .GlobalEnv)
assign("H", H, envir = .GlobalEnv)
assign("TB", TB, envir = .GlobalEnv)
assign("AVG", AVG, envir = .GlobalEnv)
assign("OBP", OBP, envir = .GlobalEnv)
assign("SLG", SLG, envir = .GlobalEnv)
assign("OPS", OPS, envir = .GlobalEnv)
assign("XBH", XBH, envir = .GlobalEnv)
assign("BB_rate", BB_rate, envir = .GlobalEnv)
assign("K_rate", K_rate, envir = .GlobalEnv)
assign("SB_rate", SB_rate, envir = .GlobalEnv)
assign("GIDP_rate", GIDP_rate, envir = .GlobalEnv)
assign("RBI_rate", RBI_rate, envir = .GlobalEnv)
assign("ROE_rate", ROE_rate, envir = .GlobalEnv)
assign("ISO", ISO, envir = .GlobalEnv)
assign("BABIP", BABIP, envir = .GlobalEnv)

# Assign Level 2 pitching stats to global environment
assign("IP", IP, envir = .GlobalEnv)
assign("IP_standalone", IP_standalone, envir = .GlobalEnv)
assign("ERA", ERA, envir = .GlobalEnv)
assign("WHIP", WHIP, envir = .GlobalEnv)
assign("K_per_9", K_per_9, envir = .GlobalEnv)
assign("BB_per_9", BB_per_9, envir = .GlobalEnv)
assign("HR_per_9", HR_per_9, envir = .GlobalEnv)
assign("HBP_per_9", HBP_per_9, envir = .GlobalEnv)
assign("K_pct", K_pct, envir = .GlobalEnv)
assign("BB_pct", BB_pct, envir = .GlobalEnv)
assign("K_BB_ratio", K_BB_ratio, envir = .GlobalEnv)

# Assign Level 3 advanced stats to global environment
assign("FIP", FIP, envir = .GlobalEnv)
assign("OPS_plus", OPS_plus_aligned, envir = .GlobalEnv)
assign("ERA_plus", ERA_plus, envir = .GlobalEnv)
assign("WHIP_plus", WHIP_plus, envir = .GlobalEnv)
assign("wOBA", wOBA, envir = .GlobalEnv)
assign("wRAA", wRAA, envir = .GlobalEnv)
# Store player IDs alongside wBC values to ensure correct assignment
wBC_player_ids <- as.character(batting_player_ids)
assign("wBC_player_ids", wBC_player_ids, envir = .GlobalEnv)
assign("wBC", wBC, envir = .GlobalEnv)  # Ensure wBC is assigned to global environment
assign("sbWAR", sbWAR_aligned, envir = .GlobalEnv)
assign("spWAR", spWAR_aligned, envir = .GlobalEnv)
assign("gWAR", gWAR_aligned, envir = .GlobalEnv)
assign("wTWAR", wTWAR, envir = .GlobalEnv)

# Calculate wRC+ (Weighted Runs Created Plus)
# First calculate wRC (Weighted Runs Created)
# League_R_PA is the league average runs per plate appearance
League_R_PA <- safe_divide(sum(rbis, na.rm = TRUE), sum(plate_app, na.rm = TRUE), default = 0.12)

# wOBA scale for converting to runs
wOBA_scale <- League_wOBA_Scale  # Use existing wOBA scale from earlier calculation

wRC <- ((wOBA - League_wOBA) / wOBA_scale) * plate_app + (League_R_PA * plate_app)

# Calculate league average wRC per plate appearance
league_wRC <- sum(wRC, na.rm = TRUE)
league_wRC_per_PA <- safe_divide(league_wRC, sum(plate_app, na.rm = TRUE), default = 0.12)

# Calculate wRC+ (scaled so that 100 is league average)
wRC_plus <- safe_divide(wRC, plate_app, default = league_wRC_per_PA) / league_wRC_per_PA * 100

# Handle any NA or Inf values
wRC_plus[is.na(wRC_plus) | is.infinite(wRC_plus)] <- 100  # Set to league average if undefined

# Align to all players for consistent indexing
wRC_plus_vec <- setNames(wRC_plus, batting_ids)
wRC_plus_aligned <- wRC_plus_vec[as.character(all_ids)]
wRC_plus_aligned[is.na(wRC_plus_aligned)] <- 100  # Set to league average for non-batters

assign("wRC", wRC, envir = .GlobalEnv)
assign("wRC_plus", wRC_plus_aligned, envir = .GlobalEnv)

# ---- STAT EXPLANATIONS FOR ADVANCED METRICS MODULE ----

stat_explanations <- list(
  # Level 2 Stats
  AB = "At Bats (AB): The number of times a player bats, 
  not counting walks, hit by pitch, sacrifices, or interference.",
  H = "Hits (H): Total times a player reaches at least 
  first base safely by hitting the ball (includes singles, doubles, 
  triples, and home runs).",
  HR = "Tanks. Swamp Donkeys. Nukes. Number of. No further explanation needed. Bat flip. Roasted.",
  TB = "Total Bases (TB): The sum of all bases a player earns 
  from hits (single=1, double=2, triple=3, homer=4).",
  AVG = "Batting Average (AVG): Hits divided by at bats.", 
  OBP = "On-Base Percentage (OBP): How often you reach base 
  by any means (hit, walk, hit by pitch).",
  SLG = "Slugging Percentage (SLG): How many bases you 
  get per at bat. Shows your power!",
  OPS = "On-base Plus Slugging (OPS): Sounds like a fancy stat, but it's literally OBP + SLG. 
  If you're going to point to one stat to flex on your friends or opponents - look no further than here",
  RBI = "Run Batted In:  When you drive in a run with a hit, a walk, 
  or heck, even a ground out or fly out!",
  ROE = "Reached on Error: How often you get on base because the defense messed up.",
  ISO = "Isolated Power (ISO): SLG - AVG. Shows how much of 
  your hitting is for extra bases.",
  BABIP = "Batting Average on Balls in Play (BABIP): How often your 
  non-homer balls in play turn into hits.",
  IP = "Innings Pitched (IP): Outs recorded divided by 3. 
  2.1 IP means 2 innings plus 1 out.",
  ERA = "Earned Run Average (ERA): Runs allowed per 9 innings. Lower is better! 
  We only do 6 innings, so look at your **ERA+ stat** and compare to your favorite MLB players!",
  WHIP = "Walks + Hits per Inning (WHIP): How many baserunners you allow per inning.",
  K_per_9 = "Strikeouts per 9 Innings (K/9): How many batters you strike out per 9 innings.",
  BB_per_9 = "Walks per 9 Innings (BB/9): How many batters you walk per 9 innings.",
  HBP_per_9 = "Hit Batters per 9 Innings (HBP/9): How many batters you hit per 9 innings.",
  K_pct = "Strikeout Percentage (K%): Percentage of batters faced that you strike out. So a K Rate of 
  1.00 means you strike out every batter you face",
  BB_pct = "Walk Percentage (BB%): Percentage of batters faced you walk.",
  'K/BB Ratio' = "Strikeout-to-Walk Ratio (K/BB): How many batters you strike 
  out for every walk you allow.",
  
  # Level 3 Stats
  'OPS+' = "OPS+ (OPS Plus): How your OPS compares to league average 
  (100 = average. Above 100 = better. Above 200? - You're in the top 25% of hitters in the league!
  Above 300? You're probably the best hitter in the league. Beyond that, we're talking unnecessary 
  godlike abilities compared to the rest of the league. Dial it back a little bit, Muscles McGee",
  'ERA+' = "ERA+ (ERA Plus): How your ERA compares to league average. Look at this along with WHIP+. I'd probably look at them INSTEAD of traditional ERA and WHIP.
  100 = average, above 100 = better, above 200 = you are not allowing a whole lot of runs, above 300 =
  you are not even thinking about allowing any runs. 400-500+? = the other team probably isn't thinking
  about scoring any runs off of you).",
  'WHIP+' = "WHIP+ (WHIP Plus): How your WHIP compares to league average. 100 = average, above 
  100 = better - you get the idea what the '+' means now!",
  sbWAR = "Simplified Batting WAR (sbWAR): How many more wins you created with your bat than an average player, scaled to an MLB-length 
  season. They play 162, we'll play 13-15 of them",
  spWAR = "Simplified Pitching WAR (spWAR): How many more wins you 
  saved with your pitching than an average pitcher, again scaled to MLB season length",
  K_BB_ratio = "Strikeout-to-Walk Ratio (K/BB): How many batters you strike 
  out for every walk you allow.",
  wOBA = "Weighted On-Base Average (wOBA): Like OBP, but gives more credit 
  for extra-base hits. Because power is awesome, right?",
  wRC = "Weighted Runs Created (wRC): How many runs you actually created for your team.",
  'wRC+' = "Weighted Runs Created Plus (wRC+): How your run creation compares 
  to league average (100 = average).",
  FIP = "Fielding Independent Pitching (FIP) - Think of this as ERA, but if the only possible outcomes were: The batter hits 
  a Home Run (That's Really Bad), the batter walks (That's not great), the batter is hit by the pitch (That's not great and also Ouch),
  or you strike the batter out (Hey! Finally something good!)",
  
  # Gus's stats
  wBC = "Weighted Bases Created (wBC): An Advanced Metric invented by our SABRmetrician 
  (and player for the 11/12 Tigers), Gus Murphy! A one-stop-shop metric for total value 
  per plate appearance, including hitting, running, creating scoring opportunities, 
  and avoiding outs!",
  gWAR = "Gus WAR (gWAR): Gus's version of WAR, using wBC to show your overall 
  impact on winning. Also the name of a very weird heavy metal band most popular 
  in the 1990s. Ask your parents! Or, better yet...don't!",
  
  wTWAR = "Weighted Total WAR (wTWAR): Unique to our brand of youth baseball - Adds up the 
  three WAR metrics (sbWAR, spWAR, gWAR), but 
  
  a) Slightly downplays the contributions of a pitcher, because you can't pitch for a 
  whole game like you can in MLB! 
  
  b) Dominant hitters and scoring opportunity creators have a greater impact
  on the outcomes of our games
  
  c) If you do all three of those things - you are really making a difference!"
)

assign("stat_explanations",stat_explanations,envir = .GlobalEnv)

# ================================================================================================
# SECTION 8: WRAPPER FUNCTIONS FOR DATA ACCESS
# ================================================================================================

#' Get team logo path for a given team name
#'
#' @param team_name The name of the team
#' @return Path to the team's logo file
get_team_logo <- function(team_name) {
  # Normalize team name to match file naming convention
  team_name <- tolower(gsub("[[:punct:][:space:]]", "_", team_name))
  
  # Construct path to logo file
  logo_path <- file.path("www", paste0(team_name, ".png"))
  
  # Check if file exists
  if (file.exists(logo_path)) {
    return(logo_path)
  }
  
  # If not found, return default logo
  return(file.path("www", "tigers.png"))
}

#' Get player batting statistics for a specific player
#'
#' @param player_id The PlayerID to look up
#' @return List of batting statistics or NULL if not found
get_player_batting_stats <- function(player_id) {
  if (missing(player_id) || is.null(player_id) || is.na(player_id)) {
    return(NULL)
  }
  
  # Make sure player_id is numeric
  player_id <- as.numeric(as.character(player_id))
  
  # Find batting stats for this player
  batting_idx <- which(batting$PlayerID == player_id)
  if (length(batting_idx) == 0) {
    return(NULL)
  }
  
  # Aggregate batting stats for this player
  batting_stats <- list(
    PA = sum(batting$PA[batting_idx], na.rm = TRUE),
    X1B = sum(batting$X1B[batting_idx], na.rm = TRUE),
    X2B = sum(batting$X2B[batting_idx], na.rm = TRUE),
    X3B = sum(batting$X3B[batting_idx], na.rm = TRUE),
    HR = sum(batting$HR[batting_idx], na.rm = TRUE),
    BB = sum(batting$BB[batting_idx], na.rm = TRUE),
    HBP = sum(batting$HBP[batting_idx], na.rm = TRUE),
    SF = sum(batting$SF[batting_idx], na.rm = TRUE),
    SH = sum(batting$SH[batting_idx], na.rm = TRUE),
    K = sum(batting$K[batting_idx], na.rm = TRUE),
    GIDP = sum(batting$GIDP[batting_idx], na.rm = TRUE),
    SB = sum(batting$SB[batting_idx], na.rm = TRUE),
    CS = sum(batting$CS[batting_idx], na.rm = TRUE),
    ROE = sum(batting$ROE[batting_idx], na.rm = TRUE),
    RBI = sum(batting$RBI[batting_idx], na.rm = TRUE)
  )
  
  # Calculate derived stats
  batting_stats$AB <- batting_stats$PA - batting_stats$BB - batting_stats$HBP - batting_stats$SF - batting_stats$SH
  batting_stats$H <- batting_stats$X1B + batting_stats$X2B + batting_stats$X3B + batting_stats$HR
  batting_stats$AVG <- safe_batting_avg(batting_stats$H, batting_stats$AB)
  batting_stats$OBP <- safe_obp(batting_stats$H, batting_stats$BB, batting_stats$HBP, batting_stats$AB, batting_stats$SF)
  batting_stats$SLG <- safe_slg(batting_stats$X1B + 2*batting_stats$X2B + 3*batting_stats$X3B + 4*batting_stats$HR, batting_stats$AB)
  batting_stats$OPS <- batting_stats$OBP + batting_stats$SLG
  batting_stats$ISO <- batting_stats$SLG - batting_stats$AVG
  batting_stats$BABIP <- safe_divide(batting_stats$H - batting_stats$HR, batting_stats$AB - batting_stats$K - batting_stats$HR + batting_stats$SF)
  
  return(batting_stats)
}

#' Get player pitching statistics for a specific player
#'
#' @param player_id The PlayerID to look up
#' @return List of pitching statistics or NULL if not found
get_player_pitching_stats <- function(player_id) {
  if (missing(player_id) || is.null(player_id) || is.na(player_id)) {
    return(NULL)
  }
  
  # Make sure player_id is numeric
  player_id <- as.numeric(as.character(player_id))
  
  # Find pitching stats for this player
  pitching_idx <- which(pitching$PlayerID == player_id)
  if (length(pitching_idx) == 0) {
    return(NULL)
  }
  
  # Aggregate pitching stats for this player
  pitching_stats <- list(
    OutsRecorded = sum(pitching$OutsRecorded[pitching_idx], na.rm = TRUE),
    BF = sum(pitching$BF[pitching_idx], na.rm = TRUE),
    H = sum(pitching$H[pitching_idx], na.rm = TRUE),
    R = sum(pitching$R[pitching_idx], na.rm = TRUE),
    ER = sum(pitching$ER[pitching_idx], na.rm = TRUE),
    BB = sum(pitching$BB[pitching_idx], na.rm = TRUE),
    K = sum(pitching$K[pitching_idx], na.rm = TRUE),
    HBP = sum(pitching$HBP[pitching_idx], na.rm = TRUE),
    HR = sum(pitching$HR[pitching_idx], na.rm = TRUE)
  )
  
  # Calculate derived stats
  pitching_stats$IP <- pitching_stats$OutsRecorded / 3
  pitching_stats$ERA <- safe_era(pitching_stats$ER, pitching_stats$IP)
  pitching_stats$WHIP <- safe_whip(pitching_stats$BB, pitching_stats$H, pitching_stats$HBP, pitching_stats$IP)
  pitching_stats$K_per_9 <- safe_per_nine(pitching_stats$K, pitching_stats$IP)
  pitching_stats$BB_per_9 <- safe_per_nine(pitching_stats$BB, pitching_stats$IP)
  pitching_stats$HR_per_9 <- safe_per_nine(pitching_stats$HR, pitching_stats$IP)
  
  # Percentages
  pitching_stats$K_pct <- safe_divide(pitching_stats$K, pitching_stats$BF)
  pitching_stats$BB_pct <- safe_divide(pitching_stats$BB, pitching_stats$BF)
  
  # Use the new safe_k_bb_ratio function for K/BB ratio
  pitching_stats$K_BB_ratio <- safe_k_bb_ratio(pitching_stats$K, pitching_stats$BB)
  
  return(pitching_stats)
}

#' Universal player data retrieval wrapper
#'
#' @param player_id The PlayerID to look up
#' @return Named list or data frame of player stats, or NULL if not found
get_player_data <- function(player_id = NULL) {
  # If no player_id is provided, return all players
  if (missing(player_id) || is.null(player_id) || is.na(player_id)) {
    if (exists("players", envir = .GlobalEnv)) {
      return(get("players", envir = .GlobalEnv))
    } else {
      warning("get_player_data: player_id missing or invalid and no players data available")
      return(NULL)
    }
  }
  
  # Make sure player_id is numeric
  player_id <- as.numeric(as.character(player_id))
  
  # Get player info
  if (!exists("players", envir = .GlobalEnv)) {
    warning("get_player_data: players data frame not found")
    return(NULL)
  }
  
  players_data <- get("players", envir = .GlobalEnv)
  player_idx <- which(players_data$PlayerID == player_id)
  if (length(player_idx) == 0) {
    warning(sprintf("get_player_data: No data found for player_id %s", player_id))
    return(NULL)
  }
  
  # Get player stats using get_player_stats
  return(get_player_stats(player_id))
}

#' Get detailed player statistics for a specific player
#'
#' @param player_id The PlayerID to look up
#' @return Named list of player statistics
get_player_stats <- function(player_id) {
  # Make sure player_id is numeric
  player_id <- as.numeric(as.character(player_id))
  
  # Create a default stats object to use if no stats are found
  default_stats <- list(
    PlayerID = player_id,
    FirstName = "",
    LastInitial = "",
    TeamID = 0,
    # Default batting stats
    PA = 0, AB = 0, H = 0, X1B = 0, X2B = 0, X3B = 0, HR = 0, BB = 0, HBP = 0,
    SF = 0, SH = 0, K = 0, GIDP = 0, SB = 0, CS = 0, ROE = 0, RBI = 0,
    AVG = 0, OBP = 0, SLG = 0, OPS = 0, ISO = 0, BABIP = 0, OPS_plus = 100,
    wBC = 0, sbWAR = 0, gWAR = 0, wRC_plus = 100,
    # Default pitching stats
    OutsRecorded = 0, BF = 0, Pitch_H = 0, Pitch_R = 0, Pitch_ER = 0, 
    Pitch_BB = 0, Pitch_K = 0, Pitch_HBP = 0, Pitch_HR = 0, IP = 0,
    ERA = 0, WHIP = 0, K_per_9 = 0, BB_per_9 = 0, HR_per_9 = 0,
    K_pct = 0, BB_pct = 0, K_BB_ratio = 0, ERA_plus = 100, WHIP_plus = 100,
    FIP = 0, spWAR = 0,
    # Default combined stats
    wTWAR = 0
  )
  
  # Check if required data frames exist
  if (!exists("players", envir = .GlobalEnv)) {
    cat("Players data frame not found in environment\n")
    return(default_stats)
  }
  
  # Find player in players data frame
  player_idx <- which(players$PlayerID == player_id)
  if (length(player_idx) == 0) {

    return(default_stats)
  }
  
  # Start building player stats from player information
  player_stats <- default_stats
  player_stats$PlayerID <- players$PlayerID[player_idx]
  player_stats$FirstName <- players$FirstName[player_idx]
  player_stats$LastInitial <- players$LastInitial[player_idx]
  player_stats$TeamID <- players$TeamID[player_idx]
  
  # Get batting stats using the specialized function
  batting_stats <- get_player_batting_stats(player_id)
  found_batting <- !is.null(batting_stats)
  
  if (found_batting) {
    
    # Copy all batting stats fields to the combined stats
    for (field in names(batting_stats)) {
      player_stats[[field]] <- batting_stats[[field]]
    }
    
    # Add advanced metrics if available
    if (exists("OPS_plus", envir = .GlobalEnv)) {
      ops_plus_values <- get("OPS_plus", envir = .GlobalEnv)
      
      # If OPS_plus is a named vector, look up by player_id
      if (!is.null(names(ops_plus_values))) {
        player_id_char <- as.character(player_id)
        if (player_id_char %in% names(ops_plus_values)) {
          player_stats$OPS_plus <- ops_plus_values[player_id_char]
        }
      } else if (length(ops_plus_values) >= player_idx) {
        # Otherwise use the index if available
        player_stats$OPS_plus <- ops_plus_values[player_idx]
      }
    }
    
    if (exists("wOBA", envir = .GlobalEnv) && length(wOBA) >= player_idx) {
      player_stats$wOBA <- wOBA[player_idx]
    }
    
    # Calculate wRC+ directly instead of relying on global environment
    if (player_stats$PA > 0) {
      # Calculate wRC+ based on OPS if we have valid batting stats
      # This is a simplified calculation - adjust constants as needed for your league
      league_ops <- 0.710  # League average OPS - adjust based on your league
      if (exists("League_OPS", envir = .GlobalEnv)) {
        league_ops <- get("League_OPS", envir = .GlobalEnv)
      }
      
      # Scale to make 100 the league average
      player_stats$wRC_plus <- round(100 * (player_stats$OPS / league_ops))
    } else {
      player_stats$wRC_plus <- 100  # Default for players with no PAs
    }
    
    # Get wBC value by PlayerID instead of index to avoid misalignment
    if (exists("wBC", envir = .GlobalEnv) && exists("wBC_player_ids", envir = .GlobalEnv)) {
      # Find the correct index for this player in the wBC array
      player_id_char <- as.character(player_id)
      wbc_idx <- which(wBC_player_ids == player_id_char)
      if (length(wbc_idx) == 1 && wbc_idx <= length(wBC)) {
        player_stats$wBC <- wBC[wbc_idx]
      } else if (length(wBC) >= player_idx) {
        # Fallback to using player_idx if wBC_player_ids doesn't have a match
        player_stats$wBC <- wBC[player_idx]
      }
    }
  } else {
  }
  
  # Get pitching stats using the specialized function
  pitching_stats <- get_player_pitching_stats(player_id)
  found_pitching <- !is.null(pitching_stats)
  
  if (found_pitching) {
    
    # Map pitching stats to the combined stats with proper prefixes
    player_stats$OutsRecorded <- pitching_stats$OutsRecorded
    player_stats$BF <- pitching_stats$BF
    player_stats$Pitch_H <- pitching_stats$H
    player_stats$Pitch_R <- pitching_stats$R
    player_stats$Pitch_ER <- pitching_stats$ER
    player_stats$Pitch_BB <- pitching_stats$BB
    player_stats$Pitch_K <- pitching_stats$K
    player_stats$Pitch_HBP <- pitching_stats$HBP
    player_stats$Pitch_HR <- pitching_stats$HR
    player_stats$IP <- pitching_stats$IP
    player_stats$ERA <- pitching_stats$ERA
    player_stats$WHIP <- pitching_stats$WHIP
    player_stats$K_per_9 <- pitching_stats$K_per_9
    player_stats$BB_per_9 <- pitching_stats$BB_per_9
    player_stats$HR_per_9 <- pitching_stats$HR_per_9
    player_stats$K_pct <- pitching_stats$K_pct
    player_stats$BB_pct <- pitching_stats$BB_pct
    player_stats$K_BB_ratio <- pitching_stats$K_BB_ratio
    
    # Add advanced metrics if available
    if (exists("ERA_plus", envir = .GlobalEnv)) {
      player_stats$ERA_plus <- safe_era_plus(player_stats$ERA, League_ERA)
    }
    
    if (exists("WHIP_plus", envir = .GlobalEnv)) {
      player_stats$WHIP_plus <- safe_whip_plus(player_stats$WHIP, League_WHIP)
    }
    
    # Use the FIP_named vector directly - much simpler approach
    if (exists("FIP_named", envir = .GlobalEnv)) {
      # Get the named FIP vector from global environment
      fip_named_global <- get("FIP_named", envir = .GlobalEnv)
      player_id_char <- as.character(player_id)
      
      # Direct lookup by player ID
      if (player_id_char %in% names(fip_named_global)) {
        # Use the FIP value from the named vector
        player_stats$FIP <- fip_named_global[player_id_char]

      } else if (player_stats$IP > 0) {
        # Fallback: Calculate FIP directly if player not found in vector

        player_stats$FIP <- ((13 * player_stats$Pitch_HR) + 
                           (3 * (player_stats$Pitch_BB + player_stats$Pitch_HBP)) - 
                           (2 * player_stats$Pitch_K)) / player_stats$IP
        
        # Cap extremely high FIPs
        player_stats$FIP <- min(player_stats$FIP, 99.99)
      } else {
        player_stats$FIP <- 0.00  # Default FIP for pitchers with no IP
      }
    } else if (player_stats$IP > 0) {
      # Fallback: Calculate FIP directly if FIP_named vector not available

      player_stats$FIP <- ((13 * player_stats$Pitch_HR) + 
                         (3 * (player_stats$Pitch_BB + player_stats$Pitch_HBP)) - 
                         (2 * player_stats$Pitch_K)) / player_stats$IP
      
      # Cap extremely high FIPs
      player_stats$FIP <- min(player_stats$FIP, 99.99)
    } else {
      player_stats$FIP <- 0.00  # Default FIP for pitchers with no IP
    }
    
    # WAR values will be added later using the centralized get_player_war function
  } else {
  }
  
  # Get all WAR-related values from the centralized get_player_war function
  if (exists("get_player_war", envir = .GlobalEnv)) {
    # Use the centralized function to get consistent WAR values
    war_stats <- get_player_war(player_id)
    
    # Add all WAR values to player_stats
    player_stats$sbWAR <- war_stats$sbWAR
    player_stats$spWAR <- war_stats$spWAR
    player_stats$gWAR <- war_stats$gWAR
    player_stats$wTWAR <- war_stats$wTWAR
    
    # Log the WAR values for debugging

  } else {
    # Fallback to default values if centralized function is not available
    player_stats$sbWAR <- 0
    player_stats$spWAR <- -0.1  # Default non-pitcher penalty
    player_stats$gWAR <- 0
    player_stats$wTWAR <- 0
    

  }
  
  # Return the combined player stats
  return(player_stats)
}

#' Get team data
#' @return Data frame of team data or NULL if not found
get_team_data <- function() {
  if (!exists("teams", envir = .GlobalEnv)) {
    warning("get_team_data: teams data frame not found")
    return(NULL)
  }
  
  return(get("teams", envir = .GlobalEnv))
}

#' Aggregate player-level batting stats for league leaderboards
#' @param min_pa Minimum plate appearances to include (default 0)
#' @return Data frame of player batting stats with derived metrics
get_batting_data <- function(min_pa = 0) {
  # Debug logging

  
  if (!exists("batting", envir = .GlobalEnv)) {
    warning("get_batting_data: batting data frame not found")
    return(data.frame())
  }
  
  # Get unique player IDs
  ids <- unique(batting$PlayerID)

  
  # Create empty list to hold results
  player_stats_list <- list()
  valid_count <- 0
  
  # Build data frame from individual player stats
  for (id in ids) {
    # Get stats for this player using the specialized function
    player_batting_stats <- get_player_batting_stats(id)
    
    # Only include players with minimum plate appearances
    if (!is.null(player_batting_stats) && player_batting_stats$PA >= min_pa) {
      valid_count <- valid_count + 1
      
      # Add player ID to the stats list
      player_batting_stats$PlayerID <- id
      
      # Add to list
      player_stats_list[[valid_count]] <- player_batting_stats
    }
  }
  

  
  # If no valid players found, return empty data frame with expected columns
  if (valid_count == 0) {
    return(data.frame(
      PlayerID = integer(0),
      PA = integer(0),
      AB = integer(0),
      H = integer(0),
      X1B = integer(0),
      X2B = integer(0),
      X3B = integer(0),
      HR = integer(0),
      BB = integer(0),
      HBP = integer(0),
      SF = integer(0),
      SH = integer(0),
      K = integer(0),
      GIDP = integer(0),
      SB = integer(0),
      CS = integer(0),
      ROE = integer(0),
      RBI = integer(0),
      AVG = numeric(0),
      OBP = numeric(0),
      SLG = numeric(0),
      OPS = numeric(0),
      ISO = numeric(0),
      BABIP = numeric(0)
    ))
  }
  
  # Convert list of player stats to data frame
  # This is more efficient than rbinding one row at a time
  result <- do.call(rbind, lapply(player_stats_list, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
  
  # Convert result to data frame if it's not already
  if (!is.data.frame(result)) {
    result <- as.data.frame(result, stringsAsFactors = FALSE)
  }
  
  # Return the result
  return(result)
}

#' Aggregate player-level pitching stats for league leaderboards
#' @param min_outs Minimum outs recorded to include (default 0)
#' @return Data frame of player pitching stats with derived metrics
get_pitching_data <- function(min_outs = 0) {
  # Debug logging

  
  if (!exists("pitching", envir = .GlobalEnv)) {
    warning("get_pitching_data: pitching data frame not found")
    return(data.frame())
  }
  
  # Get unique player IDs
  ids <- unique(pitching$PlayerID)

  
  # Create empty list to hold results
  player_stats_list <- list()
  valid_count <- 0
  
  # Build data frame from individual player stats
  for (id in ids) {
    # Get stats for this player using the specialized function
    player_pitching_stats <- get_player_pitching_stats(id)
    
    # Only include players with minimum outs recorded
    if (!is.null(player_pitching_stats) && player_pitching_stats$OutsRecorded >= min_outs) {
      valid_count <- valid_count + 1
      
      # Add player ID to the stats list
      player_pitching_stats$PlayerID <- id
      
      # Add to list
      player_stats_list[[valid_count]] <- player_pitching_stats
    }
  }
  

  
  # If no valid players found, return empty data frame with expected columns
  if (valid_count == 0) {
    return(data.frame(
      PlayerID = integer(0),
      OutsRecorded = integer(0),
      BF = integer(0),
      H = integer(0),
      R = integer(0),
      ER = integer(0),
      BB = integer(0),
      K = integer(0),
      HBP = integer(0),
      HR = integer(0),
      IP = numeric(0),
      ERA = numeric(0),
      WHIP = numeric(0),
      K_per_9 = numeric(0),
      BB_per_9 = numeric(0),
      HR_per_9 = numeric(0),
      K_pct = numeric(0),
      BB_pct = numeric(0),
      K_BB_ratio = numeric(0)
    ))
  }
  
  # Convert list of player stats to data frame
  # This is more efficient than rbinding one row at a time
  result <- do.call(rbind, lapply(player_stats_list, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
  
  # Convert result to data frame if it's not already
  if (!is.data.frame(result)) {
    result <- as.data.frame(result, stringsAsFactors = FALSE)
  }
  
  # Return the result
  return(result)
}

#' Get team statistics by aggregating player stats
#'
#' @param team_id The TeamID to look up
#' @return Named list of team statistics or NULL if not found
get_team_stats <- function(team_id) {
  # Debug logging


  
  # Check if required data frames exist
  if (!exists("players", envir = .GlobalEnv)) {
    cat("Players data frame not found in environment\n")
    return(NULL)
  }
  
  # Find players on this team
  idx <- which(players$TeamID == team_id)
  if (length(idx) == 0) {

    return(NULL)
  }
  

  
  # Get player IDs for this team
  player_ids <- players$PlayerID[idx]
  
  # Initialize team stats with default values
  team_stats <- list(
    TeamID = team_id,
    # Default batting stats
    PA = 0, AB = 0, H = 0, X1B = 0, X2B = 0, X3B = 0, HR = 0, BB = 0, HBP = 0,
    SF = 0, SH = 0, K = 0, GIDP = 0, SB = 0, CS = 0, ROE = 0, RBI = 0,
    AVG = 0, AVGrank = 0, OBP = 0, SLG = 0, OPS = 0, ISO = 0, BABIP = 0,
    # Default pitching stats
    OutsRecorded = 0, BF = 0, Pitch_H = 0, Pitch_R = 0, Pitch_ER = 0, 
    Pitch_BB = 0, Pitch_K = 0, Pitch_HBP = 0, Pitch_HR = 0, IP = 0,
    ERA = 0, WHIP = 0, K_per_9 = 0, BB_per_9 = 0, HR_per_9 = 0,
    K_pct = 0, BB_pct = 0, K_BB_ratio = 0,
    pythag_pct = 0
  )
  
  # Aggregate batting stats for all players on the team
  team_batting <- list()
  for (field in c("PA", "AB", "H", "X1B", "X2B", "X3B", "HR", "BB", "HBP", "SF", "SH", 
                 "K", "GIDP", "SB", "CS", "ROE", "RBI")) {
    team_batting[[field]] <- 0
  }
  
  # Aggregate pitching stats for all players on the team
  team_pitching <- list()
  for (field in c("OutsRecorded", "BF", "H", "R", "ER", "BB", "K", "HBP", "HR")) {
    team_pitching[[field]] <- 0
  }
  
  # Loop through each player on the team and aggregate their stats
  for (id in player_ids) {
    # Get batting stats for this player
    player_batting <- get_player_batting_stats(id)
    if (!is.null(player_batting)) {
      for (field in names(player_batting)) {
        if (field %in% names(team_batting)) {
          team_batting[[field]] <- team_batting[[field]] + player_batting[[field]]
        }
      }
    }
    
    # Get pitching stats for this player
    player_pitching <- get_player_pitching_stats(id)
    if (!is.null(player_pitching)) {
      for (field in names(player_pitching)) {
        if (field %in% names(team_pitching)) {
          team_pitching[[field]] <- team_pitching[[field]] + player_pitching[[field]]
        }
      }
    }
  }
  
  # Transfer aggregated batting stats to team stats
  for (field in names(team_batting)) {
    team_stats[[field]] <- team_batting[[field]]
  }
  
  # Transfer aggregated pitching stats to team stats with proper prefixes
  team_stats$OutsRecorded <- team_pitching$OutsRecorded
  team_stats$BF <- team_pitching$BF
  team_stats$Pitch_H <- team_pitching$H
  team_stats$Pitch_R <- team_pitching$R
  team_stats$Pitch_ER <- team_pitching$ER
  team_stats$Pitch_BB <- team_pitching$BB
  team_stats$Pitch_K <- team_pitching$K
  team_stats$Pitch_HBP <- team_pitching$HBP
  team_stats$Pitch_HR <- team_pitching$HR
  
  # Calculate derived batting stats
  team_stats$AVG <- safe_batting_avg(team_stats$H, team_stats$AB)
  team_stats$OBP <- safe_obp(team_stats$H, team_stats$BB, team_stats$HBP, team_stats$AB, team_stats$SF)
  
  # Total bases for slugging calculation
  total_bases <- team_stats$X1B + (2 * team_stats$X2B) + (3 * team_stats$X3B) + (4 * team_stats$HR)
  team_stats$SLG <- safe_slg(total_bases, team_stats$AB)
  team_stats$OPS <- team_stats$OBP + team_stats$SLG
  team_stats$ISO <- team_stats$SLG - team_stats$AVG
  
  # BABIP calculation
  team_stats$BABIP <- safe_divide(
    team_stats$H - team_stats$HR, 
    team_stats$AB - team_stats$K - team_stats$HR + team_stats$SF, 
    default = 0
  )
  
  # Calculate derived pitching stats
  team_stats$IP <- team_stats$OutsRecorded / 3
  team_stats$ERA <- safe_era(team_stats$Pitch_ER, team_stats$IP)
  team_stats$WHIP <- safe_whip(team_stats$Pitch_BB, team_stats$Pitch_H, team_stats$Pitch_HBP, team_stats$IP)
  team_stats$K_per_9 <- safe_per_nine(team_stats$Pitch_K, team_stats$IP)
  team_stats$BB_per_9 <- safe_per_nine(team_stats$Pitch_BB, team_stats$IP)
  team_stats$HR_per_9 <- safe_per_nine(team_stats$Pitch_HR, team_stats$IP)
  
  # Percentages
  team_stats$K_pct <- safe_divide(team_stats$Pitch_K, team_stats$BF)
  team_stats$BB_pct <- safe_divide(team_stats$Pitch_BB, team_stats$BF)
  team_stats$K_BB_ratio <- safe_k_bb_ratio(team_stats$Pitch_K, team_stats$Pitch_BB)
  
  return(team_stats)
}

get_player_rankings <- function(player_id, player_team) {
  # Rankings are computed within the player's team
  idx_team <- which(players$TeamID == player_team)
  idx_player <- which(players$PlayerID == player_id)
  if (length(idx_team) == 0 || length(idx_player) == 0) return(NULL)
  # Batting ranks
  avg_rank <- rank(-AVG[idx_team], ties.method = "min")[which(idx_team == idx_player)]
  ops_rank <- rank(-OPS[idx_team], ties.method = "min")[which(idx_team == idx_player)]
  hr_rank <- rank(-home_runs[idx_team], ties.method = "min")[which(idx_team == idx_player)]
  rbi_rank <- rank(-rbis[idx_team], ties.method = "min")[which(idx_team == idx_player)]
  # Pitching ranks (if player has pitching stats)
  era_rank <- rank(ERA[idx_team], ties.method = "min")[which(idx_team == idx_player)]
  whip_rank <- rank(WHIP[idx_team], ties.method = "min")[which(idx_team == idx_player)]
  k_rank <- rank(-K_per_9[idx_team], ties.method = "min")[which(idx_team == idx_player)]
  list(
    avg = avg_rank,
    ops = ops_rank,
    hr = hr_rank,
    rbi = rbi_rank,
    era = era_rank,
    whip = whip_rank,
    k = k_rank
  )
}

get_team_rankings <- function(team_id, division_id) {
  # Rankings are computed within the division
  idx_division <- which(teams$DivisionID == division_id)
  team_ids <- teams$TeamID[idx_division]
  idx_team <- which(team_ids == team_id)
  if (length(idx_division) == 0 || length(idx_team) == 0) return(NULL)
  # Aggregate team stats for all teams in division
  team_AVG <- sapply(team_ids, function(tid) safe_divide(sum(H[players$TeamID == tid], na.rm=TRUE), sum(AB[players$TeamID == tid], na.rm=TRUE)))
  team_OPS <- sapply(team_ids, function(tid) safe_divide(sum(H[players$TeamID == tid], na.rm=TRUE) + sum(walks[players$TeamID == tid], na.rm=TRUE) + sum(hit_by_pitch[players$TeamID == tid], na.rm=TRUE), sum(AB[players$TeamID == tid], na.rm=TRUE) + sum(walks[players$TeamID == tid], na.rm=TRUE) + sum(hit_by_pitch[players$TeamID == tid], na.rm=TRUE) + sum(sac_flies[players$TeamID == tid], na.rm=TRUE)) + safe_divide(sum(singles[players$TeamID == tid], na.rm=TRUE) + 2*sum(doubles[players$TeamID == tid], na.rm=TRUE) + 3*sum(triples[players$TeamID == tid], na.rm=TRUE) + 4*sum(home_runs[players$TeamID == tid], na.rm=TRUE), sum(AB[players$TeamID == tid], na.rm=TRUE)))
  team_runs <- sapply(team_ids, function(tid) sum(rbis[players$TeamID == tid], na.rm=TRUE))
  team_ERA <- sapply(team_ids, function(tid) safe_divide(9 * sum(earned_runs[players$TeamID == tid], na.rm=TRUE), safe_divide(sum(outs_recorded[players$TeamID == tid], na.rm=TRUE), 3)))
  team_WHIP <- sapply(team_ids, function(tid) safe_divide(sum(walks_allowed[players$TeamID == tid], na.rm=TRUE) + sum(hits_allowed[players$TeamID == tid], na.rm=TRUE), safe_divide(sum(outs_recorded[players$TeamID == tid], na.rm=TRUE), 3)))
  team_K_per_9 <- sapply(team_ids, function(tid) safe_divide(9 * sum(batters_struck_out[players$TeamID == tid], na.rm=TRUE), safe_divide(sum(outs_recorded[players$TeamID == tid], na.rm=TRUE), 3)))
  # Batting ranks
  avg_rank <- rank(-team_AVG, ties.method = "min")[idx_team]
  ops_rank <- rank(-team_OPS, ties.method = "min")[idx_team]
  run_rank <- rank(-team_runs, ties.method = "min")[idx_team]
  # Pitching ranks
  era_rank <- rank(team_ERA, ties.method = "min")[idx_team]
  whip_rank <- rank(team_WHIP, ties.method = "min")[idx_team]
  k_rank <- rank(-team_K_per_9, ties.method = "min")[idx_team]
  list(
    batting = list(
      avg = avg_rank,
      ops = ops_rank,
      runs = run_rank
    ),
    pitching = list(
      era = era_rank,
      whip = whip_rank,
      k = k_rank
    )
  )
}

#' Create master tables for all statistics
#' 
#' This function creates comprehensive master tables for batting, pitching, and WAR statistics
#' at both the player and team levels. These tables are stored in the global environment
#' for use by all modules in the application.
#' 
#' @return Invisibly returns a list of the created tables
#' @export
create_master_tables <- function() {
  # Debug logging

  
  # Create empty master tables with proper structure for player stats
  master_batting <- data.frame(
    PlayerID = integer(0),
    Player = character(0),
    Team = character(0),
    TeamID = integer(0),
    PA = integer(0),
    AB = integer(0),
    H = integer(0),
    X1B = integer(0),
    X2B = integer(0),
    X3B = integer(0),
    HR = integer(0),
    BB = integer(0),
    HBP = integer(0),
    SF = integer(0),
    SH = integer(0),
    K = integer(0),
    GIDP = integer(0),
    SB = integer(0),
    CS = integer(0),
    ROE = integer(0),
    RBI = integer(0),
    AVG = numeric(0),
    OBP = numeric(0),
    SLG = numeric(0),
    OPS = numeric(0),
    ISO = numeric(0),
    BABIP = numeric(0),
    OPS_plus = numeric(0),
    wBC = numeric(0),
    wRC_plus = numeric(0),
    stringsAsFactors = FALSE
  )
  
  master_pitching <- data.frame(
    PlayerID = integer(0),
    Player = character(0),
    Team = character(0),
    TeamID = integer(0),
    OutsRecorded = integer(0),
    BF = integer(0),
    Pitch_H = integer(0),
    Pitch_R = integer(0),
    Pitch_ER = integer(0),
    Pitch_BB = integer(0),
    Pitch_K = integer(0),
    Pitch_HBP = integer(0),
    Pitch_HR = integer(0),
    IP = numeric(0),
    ERA = numeric(0),
    WHIP = numeric(0),
    K_per_9 = numeric(0),
    BB_per_9 = numeric(0),
    HR_per_9 = numeric(0),
    K_pct = numeric(0),
    BB_pct = numeric(0),
    K_BB_ratio = numeric(0),
    ERA_plus = numeric(0),
    WHIP_plus = numeric(0),
    FIP = numeric(0),
    stringsAsFactors = FALSE
  )
  
  master_war <- data.frame(
    PlayerID = integer(0),
    Player = character(0),
    Team = character(0),
    TeamID = integer(0),
    sbWAR = numeric(0),
    spWAR = numeric(0),
    gWAR = numeric(0),
    oWAR = numeric(0),
    wTWAR = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Create empty master tables for team stats
  master_team_batting <- data.frame(
    TeamID = integer(0),
    Team = character(0),
    Games = integer(0),
    PA = integer(0),
    AB = integer(0),
    R = integer(0),
    H = integer(0),
    X1B = integer(0),
    X2B = integer(0),
    X3B = integer(0),
    HR = integer(0),
    BB = integer(0),
    HBP = integer(0),
    SF = integer(0),
    SH = integer(0),
    K = integer(0),
    GIDP = integer(0),
    SB = integer(0),
    CS = integer(0),
    ROE = integer(0),
    RBI = integer(0),
    AVG = numeric(0),
    OBP = numeric(0),
    SLG = numeric(0),
    OPS = numeric(0),
    ISO = numeric(0),
    BABIP = numeric(0),
    wRC = numeric(0),
    stringsAsFactors = FALSE
  )
  
  master_team_pitching <- data.frame(
    TeamID = integer(0),
    Team = character(0),
    Games = integer(0),
    OutsRecorded = integer(0),
    BF = integer(0),
    Pitch_H = integer(0),
    Pitch_R = integer(0),
    Pitch_ER = integer(0),
    Pitch_BB = integer(0),
    Pitch_K = integer(0),
    Pitch_HBP = integer(0),
    Pitch_HR = integer(0),
    IP = numeric(0),
    ERA = numeric(0),
    WHIP = numeric(0),
    K_per_9 = numeric(0),
    BB_per_9 = numeric(0),
    HR_per_9 = numeric(0),
    K_pct = numeric(0),
    BB_pct = numeric(0),
    K_BB_ratio = numeric(0),
    FIP = numeric(0),
    stringsAsFactors = FALSE
  )
  
  master_team_war <- data.frame(
    TeamID = integer(0),
    Team = character(0),
    sbWAR = numeric(0),
    spWAR = numeric(0),
    gWAR = numeric(0),
    oWAR = numeric(0),
    wTWAR = numeric(0),
    twTWARbyTe = numeric(0), 
    stringsAsFactors = FALSE
  )
  
  master_team_standings <- data.frame(
    TeamID = integer(0),
    Team = character(0),
    W = integer(0),
    L = integer(0),
    T = integer(0),
    PCT = numeric(0),
    GB = numeric(0),
    RS = integer(0),
    RA = integer(0),
    RD = integer(0),
    pythag_pct = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Check if required data frames exist
  if (!exists("players", envir = .GlobalEnv)) {
    warning("create_master_tables: players data frame not found")
    return(invisible(NULL))
  }
  
  if (!exists("teams", envir = .GlobalEnv)) {
    warning("create_master_tables: teams data frame not found")
    return(invisible(NULL))
  }
  
  # Get all players and teams
  all_players <- get("players", envir = .GlobalEnv)
  all_teams <- get("teams", envir = .GlobalEnv)
  

  
  # Process all players
  for (i in seq_len(nrow(all_players))) {
    player_id <- all_players$PlayerID[i]
    player_name <- paste(all_players$FirstName[i], all_players$LastInitial[i])
    team_id <- all_players$TeamID[i]
    
    # Get team name from teams data frame instead of players data frame
    team_name <- "Unknown"
    team_idx <- which(all_teams$TeamID == team_id)
    if (length(team_idx) > 0) {
      team_name <- all_teams$TeamName[team_idx[1]]
    } else {
      # Fallback to Team column if it exists
      if ("Team" %in% names(all_players)) {
        team_name <- all_players$Team[i]
      }
    }
    
    # Skip processing if any required field is NA
    if (is.na(player_id) || is.na(player_name) || is.na(team_id)) {

      next
    }
    

    
    # Get player stats safely with error handling
    tryCatch({
      # Use the existing get_player_stats function
      player_data <- get_player_stats(player_id)
      
      if (!is.null(player_data)) {
        # Create batting row with all needed stats
        batting_row <- data.frame(
          PlayerID = player_id,
          Player = player_name,
          Team = team_name,
          TeamID = team_id,
          PA = player_data$PA,
          AB = player_data$AB,
          H = player_data$H,
          X1B = player_data$X1B,
          X2B = player_data$X2B,
          X3B = player_data$X3B,
          HR = player_data$HR,
          BB = player_data$BB,
          HBP = player_data$HBP,
          SF = player_data$SF,
          SH = player_data$SH,
          K = player_data$K,
          GIDP = player_data$GIDP,
          SB = player_data$SB,
          CS = player_data$CS,
          ROE = player_data$ROE,
          RBI = player_data$RBI,
          AVG = player_data$AVG,
          OBP = player_data$OBP,
          SLG = player_data$SLG,
          OPS = player_data$OPS,
          ISO = player_data$ISO,
          BABIP = player_data$BABIP,
          OPS_plus = player_data$OPS_plus,
          wBC = player_data$wBC,
          wRC_plus = player_data$wRC_plus,
          stringsAsFactors = FALSE
        )
        
        # Create pitching row with all needed stats
        pitching_row <- data.frame(
          PlayerID = player_id,
          Player = player_name,
          Team = team_name,
          TeamID = team_id,
          OutsRecorded = player_data$OutsRecorded,
          BF = player_data$BF,
          Pitch_H = player_data$Pitch_H,
          Pitch_R = player_data$Pitch_R,
          Pitch_ER = player_data$Pitch_ER,
          Pitch_BB = player_data$Pitch_BB,
          Pitch_K = player_data$Pitch_K,
          Pitch_HBP = player_data$Pitch_HBP,
          Pitch_HR = player_data$Pitch_HR,
          IP = player_data$IP,
          ERA = player_data$ERA,
          WHIP = player_data$WHIP,
          K_per_9 = player_data$K_per_9,
          BB_per_9 = player_data$BB_per_9,
          HR_per_9 = player_data$HR_per_9,
          K_pct = player_data$K_pct,
          BB_pct = player_data$BB_pct,
          K_BB_ratio = player_data$K_BB_ratio,
          ERA_plus = player_data$ERA_plus,
          WHIP_plus = player_data$WHIP_plus,
          FIP = player_data$FIP,
          stringsAsFactors = FALSE
        )
        
        # Create WAR row with all needed stats
        war_row <- data.frame(
          PlayerID = player_id,
          Player = player_name,
          Team = team_name,
          TeamID = team_id,
          sbWAR = player_data$sbWAR,
          spWAR = player_data$spWAR,
          gWAR = player_data$gWAR,
          oWAR = player_data$sbWAR + player_data$gWAR, # Offensive WAR (batting + fielding)
          wTWAR = (0.4 * player_data$sbWAR) + (0.3 * player_data$spWAR) + (0.3 * player_data$gWAR), # Weighted Total WAR (40/30/30 split)
          stringsAsFactors = FALSE
        )
        
        # Add to our data frames
        master_batting <- rbind(master_batting, batting_row)
        master_pitching <- rbind(master_pitching, pitching_row)
        master_war <- rbind(master_war, war_row)
      }
    }, error = function(e) {
      # Log error but continue processing

    })
  }
  
  # Filter out rows with NA or 0 values for key stats
  master_batting <- master_batting[!is.na(master_batting$PA) & master_batting$PA > 0, ]
  master_pitching <- master_pitching[!is.na(master_pitching$OutsRecorded) & master_pitching$OutsRecorded > 0, ]
  
  # Create team-level aggregated statistics
  # Process each team
  for (i in seq_len(nrow(all_teams))) {
    team_id <- all_teams$TeamID[i]
    team_name <- all_teams$TeamName[i]
    
    # Skip processing if any required field is NA
    if (is.na(team_id) || is.na(team_name)) {

      next
    }
    

    
    # Get team batting stats by aggregating player stats
    team_batting <- master_batting[master_batting$TeamID == team_id, ]
    if (nrow(team_batting) > 0) {
      # Aggregate batting stats
      team_batting_row <- data.frame(
        TeamID = team_id,
        Team = team_name,
        Games = 0, # Will be filled from games data if available
        PA = sum(team_batting$PA, na.rm = TRUE),
        AB = sum(team_batting$AB, na.rm = TRUE),
        R = 0, # Will be filled from games data if available
        H = sum(team_batting$H, na.rm = TRUE),
        X1B = sum(team_batting$X1B, na.rm = TRUE),
        X2B = sum(team_batting$X2B, na.rm = TRUE),
        X3B = sum(team_batting$X3B, na.rm = TRUE),
        HR = sum(team_batting$HR, na.rm = TRUE),
        BB = sum(team_batting$BB, na.rm = TRUE),
        HBP = sum(team_batting$HBP, na.rm = TRUE),
        SF = sum(team_batting$SF, na.rm = TRUE),
        SH = sum(team_batting$SH, na.rm = TRUE),
        K = sum(team_batting$K, na.rm = TRUE),
        GIDP = sum(team_batting$GIDP, na.rm = TRUE),
        SB = sum(team_batting$SB, na.rm = TRUE),
        CS = sum(team_batting$CS, na.rm = TRUE),
        ROE = sum(team_batting$ROE, na.rm = TRUE),
        RBI = sum(team_batting$RBI, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      
      # Calculate derived stats
      team_batting_row$AVG <- if (team_batting_row$AB > 0) team_batting_row$H / team_batting_row$AB else 0
      team_batting_row$OBP <- if ((team_batting_row$AB + team_batting_row$BB + team_batting_row$HBP + team_batting_row$SF) > 0) 
        (team_batting_row$H + team_batting_row$BB + team_batting_row$HBP) / 
        (team_batting_row$AB + team_batting_row$BB + team_batting_row$HBP + team_batting_row$SF) else 0
      team_batting_row$SLG <- if (team_batting_row$AB > 0) 
        (team_batting_row$X1B + 2*team_batting_row$X2B + 3*team_batting_row$X3B + 4*team_batting_row$HR) / team_batting_row$AB else 0
      team_batting_row$OPS <- team_batting_row$OBP + team_batting_row$SLG
      team_batting_row$ISO <- team_batting_row$SLG - team_batting_row$AVG
      team_batting_row$BABIP <- if ((team_batting_row$AB - team_batting_row$K - team_batting_row$HR + team_batting_row$SF) > 0) 
        (team_batting_row$H - team_batting_row$HR) / 
        (team_batting_row$AB - team_batting_row$K - team_batting_row$HR + team_batting_row$SF) else 0
      
      # Add to master team batting table
      master_team_batting <- rbind(master_team_batting, team_batting_row)
    }
    
    # Get team pitching stats by aggregating player stats
    team_pitching <- master_pitching[master_pitching$TeamID == team_id, ]
    if (nrow(team_pitching) > 0) {
      # Aggregate pitching stats
      team_pitching_row <- data.frame(
        TeamID = team_id,
        Team = team_name,
        Games = 0, # Will be filled from games data if available
        OutsRecorded = sum(team_pitching$OutsRecorded, na.rm = TRUE),
        BF = sum(team_pitching$BF, na.rm = TRUE),
        Pitch_H = sum(team_pitching$Pitch_H, na.rm = TRUE),
        Pitch_R = sum(team_pitching$Pitch_R, na.rm = TRUE),
        Pitch_ER = sum(team_pitching$Pitch_ER, na.rm = TRUE),
        Pitch_BB = sum(team_pitching$Pitch_BB, na.rm = TRUE),
        Pitch_K = sum(team_pitching$Pitch_K, na.rm = TRUE),
        Pitch_HBP = sum(team_pitching$Pitch_HBP, na.rm = TRUE),
        Pitch_HR = sum(team_pitching$Pitch_HR, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      
      # Calculate derived stats
      team_pitching_row$IP <- team_pitching_row$OutsRecorded / 3
      # Use standard 9-inning ERA calculation for consistency with player stats
      team_pitching_row$ERA <- if (team_pitching_row$IP > 0) 
        (team_pitching_row$Pitch_ER * 9) / team_pitching_row$IP else 99.99
      # Use consistent WHIP calculation that includes HBP, matching player stats
      team_pitching_row$WHIP <- if (team_pitching_row$IP > 0) 
        (team_pitching_row$Pitch_H + team_pitching_row$Pitch_BB + team_pitching_row$Pitch_HBP) / team_pitching_row$IP else 99.99
      team_pitching_row$K_per_9 <- if (team_pitching_row$IP > 0) 
        (team_pitching_row$Pitch_K * 9) / team_pitching_row$IP else 0
      team_pitching_row$BB_per_9 <- if (team_pitching_row$IP > 0) 
        (team_pitching_row$Pitch_BB * 9) / team_pitching_row$IP else 99.99
      team_pitching_row$HR_per_9 <- if (team_pitching_row$IP > 0) 
        (team_pitching_row$Pitch_HR * 9) / team_pitching_row$IP else 99.99
      team_pitching_row$K_pct <- if (team_pitching_row$BF > 0) 
        team_pitching_row$Pitch_K / team_pitching_row$BF else 0
      team_pitching_row$BB_pct <- if (team_pitching_row$BF > 0) 
        team_pitching_row$Pitch_BB / team_pitching_row$BF else 0
      team_pitching_row$K_BB_ratio <- if (team_pitching_row$Pitch_BB > 0) 
        team_pitching_row$Pitch_K / team_pitching_row$Pitch_BB else 0
      
      # Calculate FIP
      team_pitching_row$FIP <- if (team_pitching_row$IP > 0) 
        ((13 * team_pitching_row$Pitch_HR) + 
           (3 * (team_pitching_row$Pitch_BB + team_pitching_row$Pitch_HBP)) - 
           (2 * team_pitching_row$Pitch_K)) / team_pitching_row$IP + 3.2 else 99.99
      
      # Add to master team pitching table
      master_team_pitching <- rbind(master_team_pitching, team_pitching_row)
    }
    
    # Get team WAR stats by aggregating player stats
    team_war <- master_war[master_war$TeamID == team_id, ]
    if (nrow(team_war) > 0) {
      # Aggregate WAR stats
      team_war_row <- data.frame(
        TeamID = team_id,
        Team = team_name,
        sbWAR = sum(team_war$sbWAR, na.rm = TRUE),
        spWAR = sum(team_war$spWAR, na.rm = TRUE),
        gWAR = sum(team_war$gWAR, na.rm = TRUE),
        oWAR = sum(team_war$oWAR, na.rm = TRUE),
        wTWAR = sum(team_war$wTWAR, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      
      # Add to master team WAR table
      master_team_war <- rbind(master_team_war, team_war_row)
    }
  }
  
  # Create team standings if games data is available
  if (exists("games", envir = .GlobalEnv)) {
    games_data <- get("games", envir = .GlobalEnv)
    
    # Process games data to create standings
    if (nrow(games_data) > 0) {
      # Initialize standings data
      standings <- data.frame(
        TeamID = all_teams$TeamID,
        Team = all_teams$TeamName,
        W = 0,
        L = 0,
        T = 0,
        RS = 0,
        RA = 0,
        RD = 0,
        PCT = 0,
        pythag_pct = 0,
        GB = 0,
        stringsAsFactors = FALSE
      )
      
      # Process each game
      for (i in seq_len(nrow(games_data))) {
        game <- games_data[i, ]
        
        # Skip games without scores
        if (is.na(game$HomeScore) || is.na(game$AwayScore)) {
          next
        }
        
        # Update home team stats
        home_idx <- which(standings$TeamID == game$HomeTeamID)
        if (length(home_idx) > 0) {
          standings$RS[home_idx] <- standings$RS[home_idx] + game$HomeScore
          standings$RA[home_idx] <- standings$RA[home_idx] + game$AwayScore
          
          if (game$HomeScore > game$AwayScore) {
            standings$W[home_idx] <- standings$W[home_idx] + 1
          } else if (game$HomeScore < game$AwayScore) {
            standings$L[home_idx] <- standings$L[home_idx] + 1
          } else {
            standings$T[home_idx] <- standings$T[home_idx] + 1
          }
        }
        
        # Update away team stats
        away_idx <- which(standings$TeamID == game$AwayTeamID)
        if (length(away_idx) > 0) {
          standings$RS[away_idx] <- standings$RS[away_idx] + game$AwayScore
          standings$RA[away_idx] <- standings$RA[away_idx] + game$HomeScore
          
          if (game$AwayScore > game$HomeScore) {
            standings$W[away_idx] <- standings$W[away_idx] + 1
          } else if (game$AwayScore < game$HomeScore) {
            standings$L[away_idx] <- standings$L[away_idx] + 1
          } else {
            standings$T[away_idx] <- standings$T[away_idx] + 1
          }
        }
      }
      
      # Calculate derived standings stats
      standings$PCT <- ifelse(standings$W + standings$L + standings$T > 0,
                              (standings$W + 0.5 * standings$T) / (standings$W + standings$L + standings$T),
                              0)
      standings$RD <- standings$RS - standings$RA
      
      # Calculate Pythagorean winning percentage
      # Using the standard exponent of 2 for the formula: (RS^2) / (RS^2 + RA^2)
      standings$pythag_pct <- ifelse(standings$RS + standings$RA > 0,
                                     (standings$RS^2) / (standings$RS^2 + standings$RA^2),
                                     0)
      
      # Sort by winning percentage
      standings <- standings[order(standings$PCT, decreasing = TRUE), ]
      
      # Calculate games behind
      if (nrow(standings) > 0) {
        leader_wins <- standings$W[1]
        leader_losses <- standings$L[1]
        
        for (i in seq_len(nrow(standings))) {
          if (i == 1) {
            standings$GB[i] <- 0
          } else {
            team_wins <- standings$W[i]
            team_losses <- standings$L[i]
            standings$GB[i] <- ((leader_wins - team_wins) + (team_losses - leader_losses)) / 2
          }
        }
      }
      
      # Add to master team standings table
      master_team_standings <- standings
      
      # Update games played in team batting and pitching tables
      for (i in seq_len(nrow(master_team_batting))) {
        team_id <- master_team_batting$TeamID[i]
        idx <- which(standings$TeamID == team_id)
        if (length(idx) > 0) {
          master_team_batting$Games[i] <- standings$W[idx] + standings$L[idx] + standings$T[idx]
          master_team_batting$R[i] <- standings$RS[idx]
        }
      }
      
      for (i in seq_len(nrow(master_team_pitching))) {
        team_id <- master_team_pitching$TeamID[i]
        idx <- which(standings$TeamID == team_id)
        if (length(idx) > 0) {
          master_team_pitching$Games[i] <- standings$W[idx] + standings$L[idx] + standings$T[idx]
        }
      }
    }
  }
  
  # Log results

  
  # Store the master tables in the global environment
  # We'll make copies to ensure the original data is preserved
  assign("master_batting_stats", master_batting, envir = .GlobalEnv)
  assign("master_pitching_stats", master_pitching, envir = .GlobalEnv)
  assign("master_war_stats", master_war, envir = .GlobalEnv)
  assign("master_team_batting_stats", master_team_batting, envir = .GlobalEnv)
  assign("master_team_pitching_stats", master_team_pitching, envir = .GlobalEnv)
  assign("master_team_war_stats", master_team_war, envir = .GlobalEnv)
  assign("master_team_standings", master_team_standings, envir = .GlobalEnv)
  
  # Store backup copies for verification if needed
  assign("master_batting_stats_orig", master_batting, envir = .GlobalEnv)
  assign("master_pitching_stats_orig", master_pitching, envir = .GlobalEnv)
  
#total weighted WAR, by Team (twWARbyTe)
#component of Power Rankings, adds up each individual wTWAR for event player, grouped by Team
  twWARbyTe <- master_war_stats %>% group_by(Team) %>% summarise(sum(wTWAR))
  twWARbyTe <- as.numeric(unlist(twWARbyTe[,2]))

#Team OPS Plus (TOPSP+)
TOPSP_plus <- (master_team_batting_stats$OPS/League_OPS)*100

#Team ERA Plus
  
  # Return invisibly
  invisible(list(
    player_batting = master_batting,
    player_pitching = master_pitching,
    player_war = master_war,
    team_batting = master_team_batting,
    team_pitching = master_team_pitching,
    team_war = master_team_war,
    team_standings = master_team_standings
  ))
}
