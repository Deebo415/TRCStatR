context("Math Utilities")

# Load the module to test
source("../../R/math_utils.R")

# Tests for safe_divide function
test_that("safe_divide handles normal division correctly", {
  expect_equal(safe_divide(10, 2), 5)
  expect_equal(safe_divide(0, 5), 0)
  expect_equal(safe_divide(-10, 2), -5)
})

test_that("safe_divide handles division by zero", {
  expect_equal(safe_divide(10, 0), 0)
  expect_equal(safe_divide(10, 0, default = NA), NA)
  expect_equal(safe_divide(10, 0, default = Inf), Inf)
})

test_that("safe_divide handles NA and NULL inputs", {
  expect_equal(safe_divide(NA, 5), 0)
  expect_equal(safe_divide(10, NA), 0)
  expect_equal(safe_divide(NULL, 5), 0)
  expect_equal(safe_divide(10, NULL), 0)
})

# Tests for safe_batting_average function
test_that("safe_batting_average calculates correctly", {
  expect_equal(safe_batting_average(30, 100), 0.300)
  expect_equal(safe_batting_average(0, 10), 0.000)
})

test_that("safe_batting_average handles edge cases", {
  expect_equal(safe_batting_average(10, 0), 0.000)
  expect_equal(safe_batting_average(NA, 100), 0.000)
  expect_equal(safe_batting_average(30, NA), 0.000)
})

# Tests for safe_on_base_percentage function
test_that("safe_on_base_percentage calculates correctly", {
  expect_equal(safe_on_base_percentage(30, 15, 5, 100, 2), 0.453)
  expect_equal(safe_on_base_percentage(0, 0, 0, 10, 0), 0.000)
})

test_that("safe_on_base_percentage handles edge cases", {
  expect_equal(safe_on_base_percentage(10, 5, 2, 0, 0), 0.000)
  expect_equal(safe_on_base_percentage(NA, 15, 5, 100, 2), 0.196)
  expect_equal(safe_on_base_percentage(30, NA, 5, 100, 2), 0.330)
})

# Tests for safe_slugging_percentage function
test_that("safe_slugging_percentage calculates correctly", {
  expect_equal(safe_slugging_percentage(20, 5, 3, 2, 100), 0.400)
  expect_equal(safe_slugging_percentage(0, 0, 0, 0, 10), 0.000)
})

test_that("safe_slugging_percentage handles edge cases", {
  expect_equal(safe_slugging_percentage(20, 5, 3, 2, 0), 0.000)
  expect_equal(safe_slugging_percentage(NA, 5, 3, 2, 100), 0.200)
  expect_equal(safe_slugging_percentage(20, NA, 3, 2, 100), 0.350)
})

# Tests for safe_era function
test_that("safe_era calculates correctly", {
  expect_equal(safe_era(10, 27), 10.000)
  expect_equal(safe_era(5, 27), 5.000)
})

test_that("safe_era handles edge cases", {
  expect_equal(safe_era(10, 0), 0.000)
  expect_equal(safe_era(NA, 27), 0.000)
  expect_equal(safe_era(10, NA), 0.000)
})

# Tests for safe_whip function
test_that("safe_whip calculates correctly", {
  expect_equal(safe_whip(10, 5, 9), 1.667)
  expect_equal(safe_whip(0, 0, 9), 0.000)
})

test_that("safe_whip handles edge cases", {
  expect_equal(safe_whip(10, 5, 0), 0.000)
  expect_equal(safe_whip(NA, 5, 9), 0.556)
  expect_equal(safe_whip(10, NA, 9), 1.111)
})

# Tests for safe_ops function
test_that("safe_ops calculates correctly", {
  expect_equal(safe_ops(0.300, 0.400), 0.700)
  expect_equal(safe_ops(0.000, 0.000), 0.000)
})

test_that("safe_ops handles edge cases", {
  expect_equal(safe_ops(NA, 0.400), 0.400)
  expect_equal(safe_ops(0.300, NA), 0.300)
  expect_equal(safe_ops(NA, NA), 0.000)
})
