context("Database Connection")

# Load required libraries
library(mockery)
library(DBI)

# Load the modules to test
source("../../R/db_connection.R")
source("../../R/error_handling.R")

# Mock database connection functions
mock_dbConnect <- mock()
mock_dbDisconnect <- mock()
mock_dbGetQuery <- mock()
mock_dbExecute <- mock()

# Tests for create_db_pool function
test_that("create_db_pool creates a connection pool", {
  # Mock the pool package
  mock_poolCreate <- mock()
  
  # Replace the actual function with our mock
  with_mock(
    "pool::poolCreate" = mock_poolCreate,
    "DBI::dbConnect" = mock_dbConnect,
    {
      # Call the function
      create_db_pool("test.accdb")
      
      # Check that poolCreate was called
      expect_called(mock_poolCreate, 1)
    }
  )
})

# Tests for get_connection function
test_that("get_connection returns a connection from the pool", {
  # Mock the pool functions
  mock_poolCheckout <- mock(list(connection = "mock_connection"))
  
  # Replace the actual function with our mock
  with_mock(
    "pool::poolCheckout" = mock_poolCheckout,
    {
      # Create a mock pool
      mock_pool <- list()
      
      # Call the function
      result <- get_connection(mock_pool)
      
      # Check that poolCheckout was called
      expect_called(mock_poolCheckout, 1)
      
      # Check that we got a connection
      expect_equal(result$connection, "mock_connection")
    }
  )
})

# Tests for release_connection function
test_that("release_connection returns a connection to the pool", {
  # Mock the pool functions
  mock_poolReturn <- mock()
  
  # Replace the actual function with our mock
  with_mock(
    "pool::poolReturn" = mock_poolReturn,
    {
      # Create a mock pool and connection
      mock_pool <- list()
      mock_connection <- list()
      
      # Call the function
      release_connection(mock_pool, mock_connection)
      
      # Check that poolReturn was called
      expect_called(mock_poolReturn, 1)
    }
  )
})

# Tests for execute_query function
test_that("execute_query executes a query and returns results", {
  # Create mock connection
  mock_connection <- list()
  
  # Replace the actual function with our mock
  with_mock(
    "DBI::dbGetQuery" = function(...) return(data.frame(result = "success")),
    {
      # Call the function
      result <- execute_query(mock_connection, "SELECT * FROM test")
      
      # Check that we got results
      expect_equal(result$result, "success")
    }
  )
})

# Tests for execute_statement function
test_that("execute_statement executes a statement and returns success", {
  # Create mock connection
  mock_connection <- list()
  
  # Replace the actual function with our mock
  with_mock(
    "DBI::dbExecute" = function(...) return(1),
    {
      # Call the function
      result <- execute_statement(mock_connection, "INSERT INTO test VALUES (1)")
      
      # Check that we got success
      expect_equal(result, 1)
    }
  )
})

# Tests for with_db_connection function
test_that("with_db_connection manages connection lifecycle", {
  # Mock the pool functions
  mock_poolCheckout <- mock(list(connection = "mock_connection"))
  mock_poolReturn <- mock()
  
  # Create a test function
  test_func <- function(conn) {
    return("test_result")
  }
  
  # Replace the actual functions with our mocks
  with_mock(
    "pool::poolCheckout" = mock_poolCheckout,
    "pool::poolReturn" = mock_poolReturn,
    {
      # Create a mock pool
      mock_pool <- list()
      
      # Call the function
      result <- with_db_connection(mock_pool, test_func)
      
      # Check that poolCheckout and poolReturn were called
      expect_called(mock_poolCheckout, 1)
      expect_called(mock_poolReturn, 1)
      
      # Check that we got the expected result
      expect_equal(result, "test_result")
    }
  )
})

# Tests for error handling in with_db_connection
test_that("with_db_connection handles errors properly", {
  # Mock the pool functions
  mock_poolCheckout <- mock(list(connection = "mock_connection"))
  mock_poolReturn <- mock()
  
  # Create a test function that throws an error
  test_func <- function(conn) {
    stop("Test error")
  }
  
  # Replace the actual functions with our mocks
  with_mock(
    "pool::poolCheckout" = mock_poolCheckout,
    "pool::poolReturn" = mock_poolReturn,
    {
      # Create a mock pool
      mock_pool <- list()
      
      # Call the function and expect an error
      expect_error(with_db_connection(mock_pool, test_func))
      
      # Check that poolCheckout and poolReturn were called
      expect_called(mock_poolCheckout, 1)
      expect_called(mock_poolReturn, 1)
    }
  )
})
