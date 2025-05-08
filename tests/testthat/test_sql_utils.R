context("SQL Utilities")

# Load the modules to test
source("../../R/sql_utils.R")
source("../../R/error_handling.R")

# Tests for create_parameterized_query function
test_that("create_parameterized_query handles simple queries", {
  # Create a simple query with parameters
  query <- "SELECT * FROM Players WHERE PlayerID = ? AND TeamID = ?"
  params <- list(1, 2)
  
  # Create the parameterized query
  result <- create_parameterized_query(query, params)
  
  # Check that parameters were replaced correctly
  expect_equal(result, "SELECT * FROM Players WHERE PlayerID = 1 AND TeamID = 2")
})

test_that("create_parameterized_query handles different data types", {
  # Create a query with different parameter types
  query <- "SELECT * FROM Players WHERE PlayerName = ? AND JerseyNumber = ? AND IsActive = ? AND BirthDate = ?"
  params <- list("John Smith", 42, TRUE, as.Date("2010-05-15"))
  
  # Create the parameterized query
  result <- create_parameterized_query(query, params)
  
  # Check that parameters were replaced correctly
  expect_equal(result, "SELECT * FROM Players WHERE PlayerName = 'John Smith' AND JerseyNumber = 42 AND IsActive = TRUE AND BirthDate = '2010-05-15'")
})

test_that("create_parameterized_query handles special characters", {
  # Create a query with parameters containing special characters
  query <- "SELECT * FROM Players WHERE PlayerName = ?"
  params <- list("O'Brien; DROP TABLE Players;--")
  
  # Create the parameterized query
  result <- create_parameterized_query(query, params)
  
  # Check that special characters were escaped correctly
  expect_equal(result, "SELECT * FROM Players WHERE PlayerName = 'O''Brien; DROP TABLE Players;--'")
})

# Tests for sanitize_sql_value function
test_that("sanitize_sql_value handles different data types", {
  # Test numeric values
  expect_equal(sanitize_sql_value(42), "42")
  expect_equal(sanitize_sql_value(3.14), "3.14")
  
  # Test character values
  expect_equal(sanitize_sql_value("Hello"), "'Hello'")
  
  # Test logical values
  expect_equal(sanitize_sql_value(TRUE), "TRUE")
  expect_equal(sanitize_sql_value(FALSE), "FALSE")
  
  # Test date values
  expect_equal(sanitize_sql_value(as.Date("2025-04-01")), "'2025-04-01'")
  
  # Test NULL values
  expect_equal(sanitize_sql_value(NULL), "NULL")
  expect_equal(sanitize_sql_value(NA), "NULL")
})

test_that("sanitize_sql_value escapes special characters", {
  # Test single quotes
  expect_equal(sanitize_sql_value("O'Brien"), "'O''Brien'")
  
  # Test semicolons and other SQL injection attempts
  expect_equal(sanitize_sql_value("'; DROP TABLE Players;--"), "'''; DROP TABLE Players;--'")
})

# Tests for execute_parameterized_query function
test_that("execute_parameterized_query executes queries correctly", {
  # Create a mock connection
  mock_connection <- list()
  
  # Create a mock dbGetQuery function
  mock_dbGetQuery <- function(conn, query) {
    return(data.frame(result = "success"))
  }
  
  # Replace the actual function with our mock
  with_mock(
    "DBI::dbGetQuery" = mock_dbGetQuery,
    {
      # Call the function
      result <- execute_parameterized_query(
        mock_connection,
        "SELECT * FROM Players WHERE PlayerID = ?",
        list(1)
      )
      
      # Check that we got results
      expect_equal(result$result, "success")
    }
  )
})

# Tests for execute_parameterized_statement function
test_that("execute_parameterized_statement executes statements correctly", {
  # Create a mock connection
  mock_connection <- list()
  
  # Create a mock dbExecute function
  mock_dbExecute <- function(conn, statement) {
    return(1)  # 1 row affected
  }
  
  # Replace the actual function with our mock
  with_mock(
    "DBI::dbExecute" = mock_dbExecute,
    {
      # Call the function
      result <- execute_parameterized_statement(
        mock_connection,
        "INSERT INTO Players (PlayerName, TeamID) VALUES (?, ?)",
        list("John Smith", 1)
      )
      
      # Check that we got success
      expect_equal(result, 1)
    }
  )
})

# Tests for build_insert_query function
test_that("build_insert_query creates correct INSERT queries", {
  # Create test data
  table_name <- "Players"
  data <- list(
    PlayerName = "John Smith",
    TeamID = 1,
    JerseyNumber = 42,
    IsActive = TRUE
  )
  
  # Build the query
  result <- build_insert_query(table_name, data)
  
  # Check that the query is correct
  expect_true(grepl("^INSERT INTO Players", result))
  expect_true(grepl("PlayerName", result))
  expect_true(grepl("TeamID", result))
  expect_true(grepl("JerseyNumber", result))
  expect_true(grepl("IsActive", result))
  expect_true(grepl("'John Smith'", result))
  expect_true(grepl("1", result))
  expect_true(grepl("42", result))
  expect_true(grepl("TRUE", result))
})

# Tests for build_update_query function
test_that("build_update_query creates correct UPDATE queries", {
  # Create test data
  table_name <- "Players"
  data <- list(
    PlayerName = "John Smith",
    TeamID = 1,
    JerseyNumber = 42,
    IsActive = TRUE
  )
  where_clause <- "PlayerID = 5"
  
  # Build the query
  result <- build_update_query(table_name, data, where_clause)
  
  # Check that the query is correct
  expect_true(grepl("^UPDATE Players SET", result))
  expect_true(grepl("PlayerName = 'John Smith'", result))
  expect_true(grepl("TeamID = 1", result))
  expect_true(grepl("JerseyNumber = 42", result))
  expect_true(grepl("IsActive = TRUE", result))
  expect_true(grepl("WHERE PlayerID = 5$", result))
})

# Tests for build_select_query function
test_that("build_select_query creates correct SELECT queries", {
  # Create test data
  table_name <- "Players"
  columns <- c("PlayerID", "PlayerName", "TeamID")
  where_clause <- "TeamID = 1"
  order_by <- "PlayerName ASC"
  limit <- 10
  
  # Build the query
  result <- build_select_query(table_name, columns, where_clause, order_by, limit)
  
  # Check that the query is correct
  expect_true(grepl("^SELECT PlayerID, PlayerName, TeamID FROM Players", result))
  expect_true(grepl("WHERE TeamID = 1", result))
  expect_true(grepl("ORDER BY PlayerName ASC", result))
  expect_true(grepl("LIMIT 10$", result))
})

test_that("build_select_query handles optional parameters", {
  # Create test data
  table_name <- "Players"
  
  # Build a minimal query
  result <- build_select_query(table_name)
  
  # Check that the query is correct
  expect_equal(result, "SELECT * FROM Players")
  
  # Build a query with just columns
  result <- build_select_query(table_name, c("PlayerID", "PlayerName"))
  
  # Check that the query is correct
  expect_equal(result, "SELECT PlayerID, PlayerName FROM Players")
})
