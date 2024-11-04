calculate_main_table_data <- function(data, response_vars, grouping_var) {
    data |>
    group_by({{ grouping_var }}) |> 
    summarise(
      across(
        {{ response_vars }},
        list(
          mean = ~ round(mean(.x, na.rm = TRUE), 2),
          sd = ~ round(sd(.x, na.rm = TRUE), 2)
        ),
        .names = "{.col}-{.fn}"
      )
    ) |> 
    pivot_longer(-{{ grouping_var }}, names_to = c("items", "stat"), names_sep = "-", values_to = "values") |> 
    pivot_wider(names_from = "stat", values_from = "values") |> 
    mutate(text = paste(mean, sd, sep = "/")) |> 
    select(-mean, -sd) |> 
    pivot_wider(names_from = {{grouping_var}}, values_from = text)
}

calculate_interaction_table_data <- function(data, response_vars, grouping_var, factor_var) {
  data |>
    group_by({{factor_var}}, {{grouping_var}}) |>
    summarise(across(
      {{response_vars}},
      list(
        mean = ~ round(mean(.x, na.rm = TRUE), 2),
        sd = ~ round(sd(.x, na.rm = TRUE), 2),
        n = ~ n()
      ),
      .names = "{.col}-{.fn}"
    )) |>
    pivot_longer(
      c(-{{factor_var}}, -{{grouping_var}}),
      names_to = c("items", "stat"),
      names_sep = "-",
      values_to = "values"
    ) |>
    pivot_wider(names_from = {{grouping_var}}, values_from = values) |>
    pivot_wider(names_from = stat, values_from = c(No, Yes)) |>
    mutate(mean_diff = round(No_mean - Yes_mean, 2),
           sd_diff = round(sqrt((No_sd ^ 2 / No_n) + (Yes_sd ^ 2 / Yes_n)), 2)) |>
    mutate(
      No = paste(No_mean, No_sd, sep = "/"),
      Yes = paste(Yes_mean, Yes_sd, sep = "/"),
      Difference = paste(mean_diff, sd_diff, sep = "/")
    ) |>
    select({{factor_var}}, items, No, Yes, Difference)
  
}

calculate_percentage <- function(data, response_var, grouping_var) {
  data |>
    dplyr::count({{grouping_var}}, {{response_var}}) |>
    dplyr::group_by({{grouping_var}}) |>
    dplyr::mutate(
      N = sum(n),
      relative_frequency = n / N,
      percentage = round(relative_frequency * 100, 2)
    ) |>
    dplyr::ungroup()
}

# Helper functions for generating fake data
generate_likert_response_with_labels <- function(n, levels, mean_value = NULL, spread = NULL) {
  # Extract min and max number from the named list
  min_number <- min(as.numeric(names(levels)))
  max_number <- max(as.numeric(names(levels)))
  
  # Set default mean value to the midpoint of the scale if not specified
  if (is.null(mean_value)) {
    mean_value <- (max_number + min_number) / 2
  }
  
  # Set default spread to cover the entire range if not specified
  if (is.null(spread)) {
    spread <- (max_number - min_number) / 2  # allows the standard deviation to spread responses across the range
  }
  
  # Ensure the mean_value is within the range
  if (mean_value < min_number || mean_value > max_number) {
    stop("mean_value must be within the min_number and max_number range")
  }
  
  # Generate responses from a normal distribution centered around mean_value
  sd_value <- (max_number - min_number) / spread
  responses <- rnorm(n, mean = mean_value, sd = sd_value)
  
  # Clip responses to be within the min_number and max_number range
  responses <- pmax(pmin(round(responses), max_number), min_number)
  
  # Convert numeric responses to labeled responses using the levels vector
  labeled_responses <- sapply(responses, function(x) {
    label <- levels[[as.character(x)]]
    if (label == "") {
      return(as.character(x))
    } else {
      return(paste(x, label))
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

process_column_names <- function(names, return_statement = FALSE) {
  processed <- tibble(original_names = names) %>%
    # Remove any extra whitespace in the question number part
    mutate(original_names = str_replace_all(original_names, "K\\.\\s*(\\d+)\\.\\s*(\\d+)", "K.\\1.\\2")) %>%
    # Extract question numbers and statements
    mutate(
      question_number = str_extract(original_names, "^K\\.\\d+\\.\\d+"),
      # Extract and pad the main number and subnumber separately
      main_number = ifelse(!is.na(question_number), str_extract(question_number, "\\d+(?=\\.)"), NA_character_),
      sub_number = ifelse(!is.na(question_number), str_extract(question_number, "(?<=\\.\\d\\.)(\\d+)"), NA_character_),
      main_number_padded = ifelse(!is.na(main_number), str_pad(main_number, 2, pad = "0"), NA_character_),
      sub_number_padded = ifelse(!is.na(sub_number), str_pad(sub_number, 2, pad = "0"), NA_character_),
      # Rejoin main and sub numbers
      question_number_padded = ifelse(!is.na(question_number), str_c("k", main_number_padded, "_", sub_number_padded, sep = ""), original_names),
      # Extract the statement text inside square brackets
      statement = str_extract(original_names, "\\[(.*?)\\]")
    ) %>%
    # Group by padded question number to add indexes
    group_by(question_number_padded) %>%
    mutate(index = row_number()) %>%
    ungroup() %>%
    # Combine question numbers with indexes, use original name if no question number
    mutate(
      final_name = ifelse(!is.na(statement), 
                          paste(question_number_padded, paste0("i", str_pad(index, 2, pad = "0")), sep = "_"),
                          tolower(question_number_padded))
    )
  
  # Return based on return_statement argument
  if (return_statement) {
    processed %>%
      select(final_name, statement)
  } else {
    processed %>%
      pull(final_name)
  }
}

generate_fake_responses_df <- function(col_names, n, column_definition) {
  generated_responses_list <- list()

  for (column_name in col_names) {
    matched_definition <- NULL
    
    for (definition in column_definition) {
      if (grepl(definition$column_id, column_name)) {
        matched_definition <- definition
        break
      }
    }
    
    if (!is.null(matched_definition)) {
      if (matched_definition$type == "datetime") {
        generated_responses_list[[column_name]] <- generate_random_timestamps(
          n = n,
          start_date = matched_definition$start_date,
          end_date = matched_definition$end_date
        )
      } else if (matched_definition$type == "likert") {
        generated_responses_list[[column_name]] <- generate_likert_response_with_labels(
          n = n,
          levels = matched_definition$levels,
          mean_value = matched_definition$mean_value,
          spread = 2
        )
      }
    } else {
      generated_responses_list[[column_name]] <- rep("NA", n)
    }
  }
  
  generated_df <- as_tibble(generated_responses_list)
  
  return(generated_df)
}
