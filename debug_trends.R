# Debug script to check previous rankings and manually calculate trends

# Load the previous rankings file
prev_file <- file.path(getwd(), "previous_power_rankings.rds")
if (file.exists(prev_file)) {
  prev_data <- readRDS(prev_file)
  cat("Previous rankings file exists\n")
  cat("Type of data:", class(prev_data), "\n")
  
  if (is.vector(prev_data)) {
    cat("Previous rankings (vector):", paste(prev_data, collapse = ", "), "\n")
  } else if (is.data.frame(prev_data)) {
    cat("Previous rankings (data frame):\n")
    print(prev_data)
  } else {
    cat("Previous rankings (unknown format):\n")
    print(prev_data)
  }
} else {
  cat("Previous rankings file does not exist at:", prev_file, "\n")
}

# Create a simple function to manually calculate trends
calculate_trends <- function() {
  # Define previous and current rankings
  previous_order <- c("Cubs", "Giants", "Royals", "Orioles", "Guardians", "Tigers", "Red Sox", "Mets")
  current_order <- c("Cubs", "Royals", "Giants", "Orioles", "Red Sox", "Guardians", "Tigers", "Mets")
  
  cat("\nManual trend calculation:\n")
  cat("Previous order:", paste(previous_order, collapse = ", "), "\n")
  cat("Current order:", paste(current_order, collapse = ", "), "\n")
  
  # Calculate trends for each team
  for (team in current_order) {
    current_rank <- match(team, current_order)
    previous_rank <- match(team, previous_order)
    
    rank_change <- current_rank - previous_rank
    
    trend <- if (rank_change < 0) {
      paste("UP", abs(rank_change))
    } else if (rank_change > 0) {
      paste("DOWN", rank_change)
    } else {
      "NO CHANGE"
    }
    
    cat(sprintf("%s: Previous rank %d, Current rank %d, Change %d, Trend %s\n", 
                team, previous_rank, current_rank, rank_change, trend))
  }
}

# Run the manual calculation
calculate_trends()

# Create a fixed previous rankings file
create_fixed_file <- function() {
  # Define the previous rankings as a simple vector
  previous_rankings <- c("Cubs", "Giants", "Royals", "Orioles", "Guardians", "Tigers", "Red Sox", "Mets")
  
  # Save to a new file
  new_file <- file.path(getwd(), "fixed_previous_rankings.rds")
  saveRDS(previous_rankings, new_file)
  
  cat("\nCreated fixed previous rankings file at:", new_file, "\n")
  cat("To use this file, rename it to 'previous_power_rankings.rds'\n")
}

# Create the fixed file
create_fixed_file()
