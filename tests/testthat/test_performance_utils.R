context("Performance Utilities")

# Load the module to test
source("../../R/performance_utils.R")
source("../../R/math_utils.R")

# Tests for cache_result function
test_that("cache_result caches function results", {
  # Create a counter to track function calls
  counter <- 0
  
  # Create a test function that increments the counter
  test_func <- function() {
    counter <<- counter + 1
    return("result")
  }
  
  # Call the function with caching
  result1 <- cache_result("test_key", test_func)
  
  # Call it again with the same key
  result2 <- cache_result("test_key", test_func)
  
  # Check that the function was only called once
  expect_equal(counter, 1)
  
  # Check that both calls returned the same result
  expect_equal(result1, "result")
  expect_equal(result2, "result")
})

test_that("cache_result respects expiry time", {
  # Create a counter to track function calls
  counter <- 0
  
  # Create a test function that increments the counter
  test_func <- function() {
    counter <<- counter + 1
    return("result")
  }
  
  # Call the function with a short expiry time
  result1 <- cache_result("test_key2", test_func, expiry_seconds = 0.1)
  
  # Wait for the cache to expire
  Sys.sleep(0.2)
  
  # Call it again with the same key
  result2 <- cache_result("test_key2", test_func, expiry_seconds = 0.1)
  
  # Check that the function was called twice
  expect_equal(counter, 2)
  
  # Check that both calls returned the same result
  expect_equal(result1, "result")
  expect_equal(result2, "result")
})

# Tests for vectorized_safe_divide function
test_that("vectorized_safe_divide handles vectors correctly", {
  # Create test vectors
  numerators <- c(10, 0, 5, NA)
  denominators <- c(2, 5, 0, 10)
  
  # Call the function
  results <- vectorized_safe_divide(numerators, denominators)
  
  # Check the results
  expect_equal(results, c(5, 0, 0, 0))
})

test_that("vectorized_safe_divide handles custom default", {
  # Create test vectors
  numerators <- c(10, 0, 5, NA)
  denominators <- c(2, 5, 0, 10)
  
  # Call the function with custom default
  results <- vectorized_safe_divide(numerators, denominators, default = NA)
  
  # Check the results
  expect_equal(results, c(5, 0, NA, NA))
})

# Tests for calculate_league_averages_optimized function
test_that("calculate_league_averages_optimized calculates batting averages", {
  # Create test batting data
  batting_data <- data.frame(
    PA = c(10, 15, 20),
    AB = c(8, 12, 18),
    H = c(3, 5, 6),
    "1B" = c(2, 3, 4),
    "2B" = c(1, 1, 1),
    "3B" = c(0, 1, 0),
    HR = c(0, 0, 1),
    BB = c(2, 2, 1),
    HBP = c(0, 1, 0),
    SF = c(0, 0, 1),
    SB = c(1, 0, 2)
  )
  
  # Create test pitching data
  pitching_data <- data.frame(
    OR = c(9, 12, 15),
    ER = c(3, 4, 6),
    H = c(5, 6, 8),
    BB = c(2, 3, 4),
    K = c(4, 5, 7),
    HR = c(1, 1, 2)
  )
  
  # Call the function
  result <- calculate_league_averages_optimized(batting_data, pitching_data)
  
  # Check that we got both batting and pitching averages
  expect_true("batting" %in% names(result))
  expect_true("pitching" %in% names(result))
  
  # Check some of the batting averages
  expect_equal(round(result$batting$League_AVG, 3), 0.368)
  expect_true("League_OBP" %in% names(result$batting))
  expect_true("League_SLG" %in% names(result$batting))
  
  # Check some of the pitching averages
  expect_true("League_ERA" %in% names(result$pitching))
  expect_true("League_WHIP" %in% names(result$pitching))
})

# Tests for calculate_player_stats_optimized function
test_that("calculate_player_stats_optimized calculates batting stats", {
  # Create test player data
  player_data <- data.frame(
    PA = c(10, 15, 20),
    AB = c(8, 12, 18),
    H = c(3, 5, 6),
    "1B" = c(2, 3, 4),
    "2B" = c(1, 1, 1),
    "3B" = c(0, 1, 0),
    HR = c(0, 0, 1),
    BB = c(2, 2, 1),
    HBP = c(0, 1, 0),
    SF = c(0, 0, 1),
    SB = c(1, 0, 2)
  )
  
  # Create test league averages
  league_averages <- list(
    batting = data.frame(
      League_AVG = 0.250,
      League_OBP = 0.320,
      League_SLG = 0.400,
      League_TB_per_PA = 0.5,
      League_BB_per_PA = 0.1,
      League_SB_per_PA = 0.05
    )
  )
  
  # Call the function
  result <- calculate_player_stats_optimized(player_data, league_averages, "batting")
  
  # Check that we got stats for each player
  expect_equal(nrow(result), 3)
  
  # Check that we calculated AVG, OBP, SLG, OPS, and WAR
  expect_true("AVG" %in% names(result))
  expect_true("OBP" %in% names(result))
  expect_true("SLG" %in% names(result))
  expect_true("OPS" %in% names(result))
  expect_true("WAR" %in% names(result))
})

test_that("calculate_player_stats_optimized calculates pitching stats", {
  # Create test player data
  player_data <- data.frame(
    IP = c(3, 4, 5),
    ER = c(3, 4, 6),
    H = c(5, 6, 8),
    BB = c(2, 3, 4),
    K = c(4, 5, 7),
    HR = c(1, 1, 2)
  )
  
  # Create test league averages
  league_averages <- list(
    pitching = data.frame(
      League_ERA = 4.50,
      League_WHIP = 1.30,
      League_K_per_IP = 1.0,
      League_BB_per_IP = 0.5,
      League_HR_per_IP = 0.2
    )
  )
  
  # Call the function
  result <- calculate_player_stats_optimized(player_data, league_averages, "pitching")
  
  # Check that we got stats for each player
  expect_equal(nrow(result), 3)
  
  # Check that we calculated ERA, WHIP, K9, BB9, and WAR
  expect_true("ERA" %in% names(result))
  expect_true("WHIP" %in% names(result))
  expect_true("K9" %in% names(result))
  expect_true("BB9" %in% names(result))
  expect_true("WAR" %in% names(result))
})

# Tests for project_full_season_optimized function
test_that("project_full_season_optimized projects batting stats", {
  # Create test player data
  player_data <- data.frame(
    PA = c(10, 15, 20),
    AB = c(8, 12, 18),
    H = c(3, 5, 6),
    "1B" = c(2, 3, 4),
    "2B" = c(1, 1, 1),
    "3B" = c(0, 1, 0),
    HR = c(0, 0, 1),
    BB = c(2, 2, 1),
    HBP = c(0, 1, 0),
    SF = c(0, 0, 1),
    SB = c(1, 0, 2),
    RBI = c(2, 3, 5)
  )
  
  # Call the function
  result <- project_full_season_optimized(player_data, 10, 162, "batting")
  
  # Check that we got projections for each player
  expect_equal(nrow(result), 3)
  
  # Check that we projected PA, AB, H, HR, BB, and RBI
  expect_true("Projected_PA" %in% names(result))
  expect_true("Projected_AB" %in% names(result))
  expect_true("Projected_H" %in% names(result))
  expect_true("Projected_HR" %in% names(result))
  expect_true("Projected_BB" %in% names(result))
  expect_true("Projected_RBI" %in% names(result))
  
  # Check that projections are scaled correctly
  expect_equal(result$Projected_PA[1], 10 * 162/10)
  expect_equal(result$Projected_H[1], 3 * 162/10)
})

test_that("project_full_season_optimized projects pitching stats", {
  # Create test player data
  player_data <- data.frame(
    IP = c(3, 4, 5),
    ER = c(3, 4, 6),
    H = c(5, 6, 8),
    BB = c(2, 3, 4),
    K = c(4, 5, 7),
    HR = c(1, 1, 2),
    W = c(1, 0, 1),
    L = c(0, 1, 1)
  )
  
  # Call the function
  result <- project_full_season_optimized(player_data, 5, 162, "pitching")
  
  # Check that we got projections for each player
  expect_equal(nrow(result), 3)
  
  # Check that we projected IP, ER, K, BB, and W
  expect_true("Projected_IP" %in% names(result))
  expect_true("Projected_ER" %in% names(result))
  expect_true("Projected_K" %in% names(result))
  expect_true("Projected_BB" %in% names(result))
  expect_true("Projected_W" %in% names(result))
  
  # Check that projections are scaled correctly
  expect_equal(result$Projected_IP[1], 3 * 162/5)
  expect_equal(result$Projected_K[1], 4 * 162/5)
})
