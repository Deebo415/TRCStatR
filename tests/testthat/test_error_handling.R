context("Error Handling")

# Load the module to test
source("../../R/error_handling.R")

# Tests for log_error function
test_that("log_error logs errors correctly", {
  # Create a temporary log file
  temp_log <- tempfile(fileext = ".log")
  
  # Set the log file path
  options(trcstatr.log_file = temp_log)
  
  # Log an error
  log_error("Test error message", "test_function")
  
  # Read the log file
  log_content <- readLines(temp_log)
  
  # Check that the error was logged
  expect_true(any(grepl("Test error message", log_content)))
  expect_true(any(grepl("test_function", log_content)))
  
  # Clean up
  file.remove(temp_log)
})

# Tests for with_error_handling function
test_that("with_error_handling returns result for successful function", {
  # Create a test function
  test_func <- function() {
    return("success")
  }
  
  # Call with error handling
  result <- with_error_handling(test_func)
  
  # Check that we got the expected result
  expect_equal(result, "success")
})

test_that("with_error_handling returns default value for failed function", {
  # Create a test function that throws an error
  test_func <- function() {
    stop("Test error")
  }
  
  # Call with error handling
  result <- with_error_handling(test_func, default_value = "default")
  
  # Check that we got the default value
  expect_equal(result, "default")
})

test_that("with_error_handling logs errors", {
  # Create a temporary log file
  temp_log <- tempfile(fileext = ".log")
  
  # Set the log file path
  options(trcstatr.log_file = temp_log)
  
  # Create a test function that throws an error
  test_func <- function() {
    stop("Test error")
  }
  
  # Call with error handling
  with_error_handling(test_func, error_message = "Custom error message", log_error = TRUE)
  
  # Read the log file
  log_content <- readLines(temp_log)
  
  # Check that the error was logged
  expect_true(any(grepl("Custom error message", log_content)))
  expect_true(any(grepl("Test error", log_content)))
  
  # Clean up
  file.remove(temp_log)
})

# Tests for notify_user function
test_that("notify_user creates notification for Shiny session", {
  # Create a mock session
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      return(list(type = type, message = message))
    }
  )
  
  # Mock the showNotification function
  mock_showNotification <- function(ui, type, duration) {
    return(list(ui = ui, type = type, duration = duration))
  }
  
  # Replace the actual function with our mock
  with_mock(
    "shiny::showNotification" = mock_showNotification,
    {
      # Call the function
      result <- notify_user("Test message", "error", mock_session)
      
      # Check that notification was created
      expect_equal(result$ui, "Test message")
      expect_equal(result$type, "error")
    }
  )
})

# Tests for validate_input function
test_that("validate_input validates correct input", {
  # Create validation rules
  rules <- list(
    name = function(x) nchar(x) >= 2,
    age = function(x) x >= 0 && x <= 120
  )
  
  # Create valid input
  input <- list(
    name = "John",
    age = 30
  )
  
  # Validate the input
  result <- validate_input(input, rules)
  
  # Check that validation passed
  expect_true(result$valid)
  expect_equal(length(result$errors), 0)
})

test_that("validate_input catches invalid input", {
  # Create validation rules
  rules <- list(
    name = function(x) nchar(x) >= 2,
    age = function(x) x >= 0 && x <= 120
  )
  
  # Create invalid input
  input <- list(
    name = "J",  # Too short
    age = 150    # Too high
  )
  
  # Validate the input
  result <- validate_input(input, rules)
  
  # Check that validation failed
  expect_false(result$valid)
  expect_equal(length(result$errors), 2)
})

# Tests for safe_call function
test_that("safe_call returns result for successful function", {
  # Create a test function
  test_func <- function() {
    return("success")
  }
  
  # Call safely
  result <- safe_call(test_func())
  
  # Check that we got the expected result
  expect_equal(result, "success")
})

test_that("safe_call returns NULL for failed function", {
  # Create a test function that throws an error
  test_func <- function() {
    stop("Test error")
  }
  
  # Call safely
  result <- safe_call(test_func())
  
  # Check that we got NULL
  expect_null(result)
})
