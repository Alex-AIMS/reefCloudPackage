validate_data <- function(data, rules) {
  result <- validate::confront(data, rules)
  invalid_rows <- violating_rows(result)
  val <- tidy_validation(result, rules)
  if (any(val$fails)) {
    saveRDS(invalid_rows,
      file = paste0(AWS_OUTPUT_PATH, "invalid_rows.rds")
    )
    stop(paste0("Validation failed: ",
      val$description[val$fails],
      collapse = "\n"
    ))
  } else {
    return(val)
  }
}

violating_rows <- function(result) {
  s <- summary(result)
  wch <- which(s$items > 1 & s$fails > 1)
  if (length(wch) == 0) {
    return(NULL)
  } else {
    return(violating(data, result[wch]))
  }
}

tidy_validation <- function(result, rules) {
  summary(result) |>
    mutate(description = description(rules)) |>
    dplyr::select(description, everything(), -name, -expression)
  }
