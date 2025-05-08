# TRCStatR Developer Guide

## Introduction
This guide provides comprehensive information for developers working on the TRCStatR baseball statistics application. It covers the application architecture, coding standards, module descriptions, and best practices for extending the system.

## Architecture Overview

TRCStatR follows a modular architecture with clear separation of concerns:

```
┌─────────────────────────────────────────────────────────────┐
│                      User Interface Layer                    │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │   UI Theme  │  │ Shiny Views │  │ Interactive Elements│  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
└───────────────────────────┬─────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│                     Application Logic Layer                  │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │ Controllers │  │ Validators  │  │ Error Handlers      │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
└───────────────────────────┬─────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│                      Data Processing Layer                   │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │ Statistics  │  │ Metrics     │  │ Report Generation   │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
└───────────────────────────┬─────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│                      Data Access Layer                       │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │ DB Connect  │  │ SQL Utils   │  │ Data Validation     │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## Core Modules

### 1. Error Handling (`error_handling.R`)
The error handling module provides a consistent approach to handling errors throughout the application.

**Key Functions:**
- `safe_call()`: Executes a function with error handling
- `safe_db_call()`: Executes a database function with error handling
- `log_error()`: Logs error information
- `log_warning()`: Logs warning information
- `log_info()`: Logs informational messages

**Usage Example:**
```r
result <- safe_call(
  function_to_call(arg1, arg2),
  error_message = "User-friendly error message",
  default_value = NA,
  log_error = TRUE
)
```

### 2. Database Connection (`db_connection.R`)
Manages database connections with connection pooling and error handling.

**Key Functions:**
- `create_db_connection()`: Creates a new database connection
- `get_connection_pool()`: Gets a connection from the pool
- `execute_query()`: Executes a SQL query with error handling
- `close_all_connections()`: Closes all open connections

**Usage Example:**
```r
result <- db_manager$execute_query(
  "SELECT * FROM players WHERE team_id = ?",
  params = list(team_id)
)
```

### 3. SQL Utilities (`sql_utils.R`)
Provides utilities for working with SQL, including query building and parameterization.

**Key Functions:**
- `build_select_query()`: Builds a SELECT query with parameters
- `build_insert_query()`: Builds an INSERT query with parameters
- `build_update_query()`: Builds an UPDATE query with parameters
- `sanitize_sql_input()`: Sanitizes input for SQL queries

**Usage Example:**
```r
query <- build_select_query(
  table = "players",
  columns = c("player_id", "name", "position"),
  where = "team_id = ?",
  order_by = "name ASC"
)
```

### 4. Math Utilities (`math_utils.R`)
Provides safe mathematical operations and baseball-specific calculations.

**Key Functions:**
- `safe_divide()`: Division with protection against division by zero
- `calculate_batting_average()`: Calculates batting average
- `calculate_on_base_percentage()`: Calculates on-base percentage
- `calculate_slugging_percentage()`: Calculates slugging percentage

**Usage Example:**
```r
avg <- calculate_batting_average(hits = 42, at_bats = 120)
```

### 5. Advanced Metrics (`advanced_metrics.R`)
Implements advanced baseball statistics calculations.

**Key Functions:**
- `calculate_woba()`: Calculates Weighted On-Base Average
- `calculate_wrc_plus()`: Calculates Weighted Runs Created Plus
- `calculate_fip()`: Calculates Fielding Independent Pitching
- `calculate_batter_war()`: Calculates Batter Wins Above Replacement
- `calculate_pitcher_war()`: Calculates Pitcher Wins Above Replacement

**Usage Example:**
```r
woba <- calculate_woba(
  bb = 15, hbp = 2, singles = 25, doubles = 10,
  triples = 2, hr = 5, ab = 120, sf = 3
)
```

### 6. UI Theme (`ui_theme.R`)
Manages the application's visual theme and styling.

**Key Functions:**
- `create_trc_theme()`: Creates a theme with specified colors
- `apply_theme()`: Applies a theme to UI elements
- `create_button()`: Creates a themed button

**Usage Example:**
```r
theme <- create_trc_theme(
  primary_color = "#1976D2",
  secondary_color = "#388E3C",
  accent_color = "#FFA000"
)
```

### 7. Visualizations (`visualizations.R`)
Creates interactive data visualizations for baseball statistics.

**Key Functions:**
- `create_batting_chart()`: Creates a batting statistics chart
- `create_pitching_chart()`: Creates a pitching statistics chart
- `create_team_performance_chart()`: Creates a team performance chart
- `create_player_comparison_chart()`: Creates a player comparison chart

**Usage Example:**
```r
plot <- create_batting_chart(
  player_data,
  x_stat = "games_played",
  y_stat = "batting_average",
  color_by = "position"
)
```

## Coding Standards

### Naming Conventions
- **Functions**: Use snake_case for function names (e.g., `calculate_batting_average()`)
- **Variables**: Use snake_case for variable names (e.g., `player_data`)
- **Constants**: Use UPPER_SNAKE_CASE for constants (e.g., `MAX_PLAYERS`)
- **Classes**: Use PascalCase for class names (e.g., `PlayerStats`)
- **Files**: Use snake_case for file names (e.g., `advanced_metrics.R`)

### Code Structure
- Each file should have a header comment explaining its purpose
- Group related functions together
- Add comments for complex logic
- Use consistent indentation (2 spaces)
- Keep functions focused on a single responsibility
- Maximum line length of 80 characters

### Error Handling
- Use the `safe_call()` function for error-prone operations
- Provide user-friendly error messages
- Log detailed error information for debugging
- Handle edge cases (empty data, division by zero, etc.)
- Validate input data before processing

### Documentation
- Document all functions with roxygen2-style comments
- Include parameter descriptions and return values
- Provide usage examples for complex functions
- Document any side effects or dependencies

## Extending the Application

### Adding a New Module
1. Create a new R file in the `R/` directory
2. Add the module's functions and documentation
3. Source the module in `app.R`
4. Update unit tests in the `tests/testthat/` directory

### Adding a New Statistic
1. Implement the calculation function in the appropriate module
2. Add validation for input parameters
3. Implement error handling
4. Add the statistic to the UI
5. Create unit tests for the new function

### Adding a New Visualization
1. Implement the visualization function in `visualizations.R`
2. Create a UI component in the appropriate module
3. Add the visualization to the reports or dashboards
4. Ensure the visualization is responsive and accessible

### Adding a New Database Table
1. Update the database schema
2. Create functions for CRUD operations
3. Add validation for the new data
4. Update the UI to support the new data
5. Add unit tests for the new functions

## Testing

### Unit Testing
- All functions should have unit tests
- Tests should cover normal operation and error cases
- Use the `testthat` package for testing
- Run tests before committing changes

### Test Data
- Use the sample data in the `data/` directory for testing
- Create specific test cases for edge cases
- Document test data and its purpose

## Performance Considerations

### Database Queries
- Use parameterized queries to prevent SQL injection
- Minimize the number of database calls
- Use appropriate indexes on database tables
- Limit the amount of data retrieved

### Calculations
- Use vectorized operations where possible
- Cache frequently used results
- Use batch processing for large datasets
- Consider parallel processing for intensive calculations

### UI Performance
- Minimize reactive dependencies
- Use pagination for large datasets
- Optimize visualizations for performance
- Load data asynchronously when possible

## Security Considerations

### Database Security
- Use parameterized queries to prevent SQL injection
- Validate all user input before using it in queries
- Use the minimum required permissions for database access
- Encrypt sensitive data

### Input Validation
- Validate all user input
- Sanitize data before using it in calculations or queries
- Implement appropriate access controls
- Log security-related events

## Deployment

### Development Environment
- Use RStudio for development
- Use version control (Git) for code management
- Document dependencies in the project file

### Production Environment
- Deploy using Shiny Server or RStudio Connect
- Set up appropriate logging
- Configure database connections for production
- Monitor application performance

## Troubleshooting

### Common Issues
- Database connection problems
- Missing dependencies
- Performance issues with large datasets
- UI rendering problems

### Debugging
- Check the application logs
- Use browser() for interactive debugging
- Test individual functions in isolation
- Verify database queries with small datasets

## Resources

### R and Shiny Resources
- [R Documentation](https://www.rdocumentation.org/)
- [Shiny Documentation](https://shiny.rstudio.com/)
- [R for Data Science](https://r4ds.had.co.nz/)
- [Advanced R](https://adv-r.hadley.nz/)

### Baseball Statistics Resources
- [FanGraphs Library](https://library.fangraphs.com/)
- [Baseball Reference](https://www.baseball-reference.com/)
- [The Book: Playing the Percentages in Baseball](https://www.amazon.com/Book-Playing-Percentages-Baseball/dp/1494260174)

## Conclusion
This developer guide provides a foundation for working with the TRCStatR application. By following these guidelines, you can maintain and extend the application in a consistent and maintainable way.
