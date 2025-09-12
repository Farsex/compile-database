filter_missing_data <- function(data) {
  if (missing(data)) {
    stop("Argument 'data' is required")
  }

  if (is.null(data)) {
    stop("Argument 'data' is required")
  }

  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a data.frame")
  }

  if (is.null(colnames(data))) {
    stop("Argument 'data' should have column names")
  }

  if (nrow(data) == 0) {
    stop("Argument 'data' must have at least one row")
  }

  mandatory_columns <- c(
    "original_binomial_name",
    "original_body_size",
    "original_age",
    "n_total"
  )

  if (any(!(mandatory_columns %in% colnames(data)))) {
    stop("Some mandatory columns are missing in 'data'")
  }

  ## Filter by taxonomy ----

  no_species <- c(
    which(is.na(data[["original_binomial_name"]])),
    which(data[["original_binomial_name"]] == "Unknown")
  )

  ## Filter by sex ----

  sex <- data[, colnames(data)[grep("number_", colnames(data))]]
  sex <- apply(sex, 1, sum, na.rm = TRUE)

  no_sex_1 <- which(sex == 0)

  sex <- apply(
    data[, "proportion_of_males", drop = FALSE],
    1,
    sum,
    na.rm = TRUE
  )
  no_sex_2 <- which(sex == 0)

  no_sex <- intersect(no_sex_1, no_sex_2)

  ## Filter by age/body-size ----

  pos <- which(data[["maturity_stage"]] == "Unknown")

  if (length(pos) > 0) {
    data[pos, "maturity_stage"] <- NA
  }

  no_age <- which(
    is.na(data[["original_body_size"]]) &
      is.na(data[["original_age"]]) &
      is.na(data[["maturity_stage"]])
  )

  row_to_rm <- unique(c(no_species, no_age, no_sex))

  if (length(row_to_rm) > 0) {
    data_filtered <- data[-row_to_rm, ]
  }

  infos <- data.frame(
    "Description" = c(
      "Initial rows",
      "Final rows",
      "Initial species",
      "Final species",
      "Filter: rows w/ no taxonomy",
      "Filter: rows w/ no sex",
      "Filter: rows w/ no age"
    ),
    "Number" = c(
      nrow(data),
      nrow(data_filtered),
      length(unique(data$"original_binomial_name")),
      length(unique(data_filtered$"original_binomial_name")),
      length(no_species),
      length(no_sex),
      length(no_age)
    )
  )

  dbname <- unique(data[["database"]])

  infos <- knitr::kable(infos, format = "simple", padding = 2)

  infos <- paste0(
    c(paste0("DATABASE: ", dbname), "", infos),
    collapse = "\n"
  )

  cat(infos, file = here::here("outputs", paste0(dbname, ".log")))

  data_filtered
}
