library(testthat)
library(shiny)

# Load application files
source("../R/dependencies.R")
ensure_packages_installed(c("testthat", "mockery", "DBI"))

# Run all tests
test_dir("testthat", reporter = "summary")
