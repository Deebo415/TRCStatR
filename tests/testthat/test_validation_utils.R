context("Validation Utilities")

# Load the modules to test
source("../../R/validation_utils.R")
source("../../R/error_handling.R")

# Tests for validate_player_data function
test_that("validate_player_data validates correct data", {
  # Create valid player data
  valid_player <- list(
    FirstName = "John",
    LastInitial = "D",
    TeamID = 1,
    JerseyNumber = 42
  )
  
  # Validate the data
  result <- validate_player_data(valid_player)
  
  # Check that validation passed
  expect_true(result$valid)
  expect_equal(length(result$errors), 0)
})

test_that("validate_player_data catches invalid data", {
  # Create invalid player data
  invalid_player <- list(
    FirstName = "J",  # Too short
    LastInitial = "Doe",  # Too long
    TeamID = "not a number",
    JerseyNumber = 100  # Too high
  )
  
  # Validate the data
  result <- validate_player_data(invalid_player)
  
  # Check that validation failed
  expect_false(result$valid)
  expect_equal(length(result$errors), 4)
})

# Tests for validate_game_data function
test_that("validate_game_data validates correct data", {
  # Create valid game data
  valid_game <- list(
    HomeTeamID = 1,
    AwayTeamID = 2,
    GameDate = as.Date("2025-04-01"),
    Location = "Field 1"
  )
  
  # Validate the data
  result <- validate_game_data(valid_game)
  
  # Check that validation passed
  expect_true(result$valid)
  expect_equal(length(result$errors), 0)
})

test_that("validate_game_data catches invalid data", {
  # Create invalid game data
  invalid_game <- list(
    HomeTeamID = "not a number",
    AwayTeamID = 1,
    GameDate = "not a date",
    Location = ""  # Empty location
  )
  
  # Validate the data
  result <- validate_game_data(invalid_game)
  
  # Check that validation failed
  expect_false(result$valid)
  expect_equal(length(result$errors), 3)
})

test_that("validate_game_data catches same team playing itself", {
  # Create game data with same home and away team
  same_team_game <- list(
    HomeTeamID = 1,
    AwayTeamID = 1,
    GameDate = as.Date("2025-04-01"),
    Location = "Field 1"
  )
  
  # Validate the data
  result <- validate_game_data(same_team_game)
  
  # Check that validation failed
  expect_false(result$valid)
  expect_true(any(grepl("same team", result$errors)))
})

# Tests for validate_batting_stats function
test_that("validate_batting_stats validates correct data", {
  # Create valid batting stats
  valid_stats <- list(
    PlayerID = 1,
    GameID = 1,
    PA = 4,
    AB = 3,
    "1B" = 1,
    "2B" = 0,
    "3B" = 0,
    HR = 1,
    BB = 1,
    HBP = 0,
    SF = 0,
    SH = 0,
    SB = 0,
    RBI = 2,
    K = 1
  )
  
  # Validate the data
  result <- validate_batting_stats(valid_stats)
  
  # Check that validation passed
  expect_true(result$valid)
  expect_equal(length(result$errors), 0)
})

test_that("validate_batting_stats catches invalid data", {
  # Create invalid batting stats
  invalid_stats <- list(
    PlayerID = "not a number",
    GameID = 1,
    PA = 3,
    AB = 4,  # AB > PA is invalid
    "1B" = 2,
    "2B" = 1,
    "3B" = 0,
    HR = 1,  # Total hits (4) > AB (3) is invalid
    BB = -1,  # Negative value is invalid
    HBP = 0,
    SF = 0,
    SH = 0,
    SB = 0,
    RBI = 2,
    K = 1
  )
  
  # Validate the data
  result <- validate_batting_stats(invalid_stats)
  
  # Check that validation failed
  expect_false(result$valid)
  expect_true(length(result$errors) >= 3)
})

# Tests for validate_pitching_stats function
test_that("validate_pitching_stats validates correct data", {
  # Create valid pitching stats
  valid_stats <- list(
    PlayerID = 1,
    GameID = 1,
    OR = 9,  # 3 innings
    ER = 2,
    R = 3,
    H = 4,
    BB = 2,
    K = 5,
    HR = 1,
    HBP = 0
  )
  
  # Validate the data
  result <- validate_pitching_stats(valid_stats)
  
  # Check that validation passed
  expect_true(result$valid)
  expect_equal(length(result$errors), 0)
})

test_that("validate_pitching_stats catches invalid data", {
  # Create invalid pitching stats
  invalid_stats <- list(
    PlayerID = "not a number",
    GameID = 1,
    OR = -3,  # Negative value is invalid
    ER = 5,
    R = 3,  # ER > R is invalid
    H = 4,
    BB = 2,
    K = 5,
    HR = 1,
    HBP = 0
  )
  
  # Validate the data
  result <- validate_pitching_stats(invalid_stats)
  
  # Check that validation failed
  expect_false(result$valid)
  expect_true(length(result$errors) >= 3)
})

# Tests for validate_team_data function
test_that("validate_team_data validates correct data", {
  # Create valid team data
  valid_team <- list(
    TeamName = "Eagles",
    DivisionID = 1,
    CoachName = "John Smith"
  )
  
  # Validate the data
  result <- validate_team_data(valid_team)
  
  # Check that validation passed
  expect_true(result$valid)
  expect_equal(length(result$errors), 0)
})

test_that("validate_team_data catches invalid data", {
  # Create invalid team data
  invalid_team <- list(
    TeamName = "E",  # Too short
    DivisionID = "not a number",
    CoachName = ""  # Empty coach name
  )
  
  # Validate the data
  result <- validate_team_data(invalid_team)
  
  # Check that validation failed
  expect_false(result$valid)
  expect_true(length(result$errors) >= 3)
})

# Tests for sanitize_input function
test_that("sanitize_input converts types correctly", {
  # Create test input
  input <- list(
    numeric_string = "123",
    text = "John Smith",
    date_string = "2025-04-01",
    boolean_string = "TRUE"
  )
  
  # Define expected types
  expected_types <- list(
    numeric_string = "numeric",
    text = "character",
    date_string = "Date",
    boolean_string = "logical"
  )
  
  # Sanitize the input
  result <- sanitize_input(input, expected_types)
  
  # Check that types were converted correctly
  expect_equal(class(result$numeric_string), "numeric")
  expect_equal(class(result$text), "character")
  expect_equal(class(result$date_string), "Date")
  expect_equal(class(result$boolean_string), "logical")
})

test_that("sanitize_input handles invalid conversions", {
  # Create test input with invalid values
  input <- list(
    bad_numeric = "not a number",
    bad_date = "not a date",
    bad_boolean = "maybe"
  )
  
  # Define expected types
  expected_types <- list(
    bad_numeric = "numeric",
    bad_date = "Date",
    bad_boolean = "logical"
  )
  
  # Define default values
  default_values <- list(
    bad_numeric = 0,
    bad_date = as.Date("2025-01-01"),
    bad_boolean = FALSE
  )
  
  # Sanitize the input
  result <- sanitize_input(input, expected_types, default_values)
  
  # Check that default values were used for invalid conversions
  expect_equal(result$bad_numeric, 0)
  expect_equal(result$bad_date, as.Date("2025-01-01"))
  expect_equal(result$bad_boolean, FALSE)
})
