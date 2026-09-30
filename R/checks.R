check_positive_numbers <- function(data) {
  columns <- c(
    "number_female",
    "number_male",
    "n_total",
    "body_length_male_mm",
    "body_length_female_mm",
    "body_mass_male_g",
    "body_mass_female_g"
  )

  for (column in columns) {
    data_no_na <- data[!is.na(data[, column]), ]
    if (nrow(data_no_na) > 0) {
      if (any(data_no_na[, column] < 0)) {
        stop("The column '", column, "' cannot contain negative values")
      }
    }
  }

  invisible(NULL)
}

check_missing_values <- function(data) {
  columns <- c(
    "number_female",
    "number_male",
    "n_total",
    "longitude",
    "latitude",
    "life_stage"
  )

  for (column in columns) {
    if (anyNA(data[, column])) {
      stop("The column '", column, "' cannot contain NA")
    }
  }

  invisible(NULL)
}


check_coords <- function(aggregated_data) {
  x_rng <- range(aggregated_data$"longitude")

  if (x_rng[1] < -180 || x_rng[2] > 180) {
    stop("Some longitudes are out of bound")
  }

  y_rng <- range(aggregated_data$"latitude")

  if (y_rng[1] < -90 || y_rng[2] > 90) {
    stop("Some latidues are out of bound")
  }

  invisible(NULL)
}
