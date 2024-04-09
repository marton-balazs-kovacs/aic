# Helper functions for generating fake data
generate_likert_response_with_labels <- function(n, min_number, max_number, min_label, max_label) {
  # Randomly generate n responses for a Likert scale question
  responses <- sample(min_number:max_number, n, replace = TRUE)
  
  # Convert numeric responses to labeled responses
  labeled_responses <- sapply(responses, function(x) {
    if (x == min_number) {
      return(paste(min_number, min_label))
    } else if (x == max_number) {
      return(paste(max_number, max_label))
    } else {
      return(as.character(x))
    }
  })
  
  return(labeled_responses)
}

generate_random_timestamps <- function(n, start_date, end_date) {
  # Convert start_date and end_date to Date objects
  start_date <- as.Date(start_date, format = "%d/%m/%Y")
  end_date <- as.Date(end_date, format = "%d/%m/%Y")
  
  # Generate n random Dates between start_date and end_date
  dates <- sample(seq(start_date, end_date, by="day"), n, replace = TRUE)
  
  # Generate n random times
  hours <- sprintf("%02d", sample(0:23, n, replace = TRUE))
  minutes <- sprintf("%02d", sample(0:59, n, replace = TRUE))
  seconds <- sprintf("%02d", sample(0:59, n, replace = TRUE))
  
  # Combine Dates and times
  timestamps <- paste(format(dates, "%d/%m/%Y"), hours, minutes, seconds, sep=" ")
  
  # Return timestamps as character strings
  return(timestamps)
}