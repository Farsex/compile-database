japan <- data.frame()

sheets <- 2:5

for (sheet in sheets) {
  dat <- readxl::read_xlsx(
    here::here(
      "data",
      "japan",
      "Organized_data_Chickengrunt_Skipjacktuna_Splendidalfonsio_Coastflyingfish_20250905.xlsx"
    ),
    sheet = sheet
  )

  dat <- as.data.frame(dat)

  new_dat <- list()

  for (i in 2:ncol(dat)) {
    new_dat[[i - 1]] <- as.data.frame(t(dat[, i]))
  }

  new_dat <- do.call(rbind.data.frame, new_dat)

  colnames(new_dat) <- dat[, 1]

  japan <- rbind(japan, new_dat)
}

filename <- gsub("\\s", "_", tolower(unique(new_dat$Species)))

# writexl::write_xlsx(new_dat, paste0(filename, ".xlsx"))

sheets <- 1:2

for (sheet in sheets) {
  dat <- readxl::read_xlsx(
    here::here(
      "data",
      "japan",
      "mackerel_organizeddata_20250905.xlsx"
    ),
    sheet = sheet
  )

  dat <- as.data.frame(dat)

  new_dat <- list()

  for (i in 2:ncol(dat)) {
    new_dat[[i - 1]] <- as.data.frame(t(dat[, i]))
  }

  new_dat <- do.call(rbind.data.frame, new_dat)

  colnames(new_dat) <- dat[, 1]

  japan <- rbind(japan, new_dat)
}


##

dat <- readxl::read_xlsx(
  here::here(
    "data",
    "japan",
    "Round herring_20250905.xlsx"
  ),
  sheet = 1
)

dat <- as.data.frame(dat)

new_dat <- list()

for (i in 2:ncol(dat)) {
  new_dat[[i - 1]] <- as.data.frame(t(dat[, i]))
}

new_dat <- do.call(rbind.data.frame, new_dat)

colnames(new_dat) <- dat[, 1]

japan <- rbind(japan, new_dat)



dat <- readxl::read_xlsx(
  here::here(
    "data",
    "japan",
    "Cobaltcap silverside_organized data_20250905.xlsx"
  ),
  sheet = 1
)

dat <- as.data.frame(dat)

new_dat <- list()

for (i in 2:ncol(dat)) {
  new_dat[[i - 1]] <- as.data.frame(t(dat[, i]))
}

new_dat <- do.call(rbind.data.frame, new_dat)

colnames(new_dat) <- dat[, 1]

japan <- rbind(japan, new_dat)


japan <- japan[japan$number_female != "2？", ]

cols <- c(
  "database",
  "reference_id",
  "original_row_identifier",
  "original_binomial_name",
  "original_taxonomy_info",
  "number_female",
  "number_male",
  "number_intersex",
  "proportion_of_males",
  "n_total",
  "sexing_method_phenotypic",
  "original_life_stage",
  "maturity_stage",
  "maturity_stage_scale",
  "number_concordant_females",
  "number_concordant_males",
  "number_reversed_females",
  "number_reversed_males",
  "number_yy_ww",
  "original_age",
  "original_age_unit",
  "original_body_size",
  "original_body_size_type",
  "original_body_size_unit",
  "original_body_mass",
  "original_body_mass_type",
  "original_body_mass_unit",
  "biological_scale",
  "latitude_start",
  "longitude_start",
  "latitude_end",
  "longitude_end",
  "asl",
  "location",
  "date_start",
  "date_end",
  "day_start",
  "day_end",
  "month_start",
  "month_end",
  "year_start",
  "year_end",
  "time_start",
  "time_end",
  "surface_temperature",
  "bottom_temperature",
  "surface_salinity",
  "bottom_salinity",
  "capture_method"
)


tmp <- as.data.frame(matrix(NA, ncol = length(cols), nrow = nrow(japan)))
colnames(tmp) <- cols

tmp$database <- "Yoji's Japan dataset"
tmp$original_row_identifier <- seq_len(nrow(japan))
tmp$original_binomial_name <- japan$Species
tmp$number_female <- japan$number_female
tmp$number_male <- japan$number_male
tmp$biological_scale <- "Individual"

tmp$original_body_size <- japan$original_body_size
tmp$original_body_size_type <- japan$original_body_size_type
tmp$original_body_size_unit <- japan$original_body_size_unit

tmp$location <- japan$location

tmp$day_start <- as.numeric(japan$day_start)
tmp$month_start <- as.numeric(japan$month_start)
tmp$year_start <- as.numeric(japan$year_start)
tmp$time_start <- paste(tmp$year_start, tmp$month_start, tmp$day_start, sep = "-")
tmp$time_start <- lubridate::ymd(tmp$time_start)

tmp$day_end <- as.numeric(japan$day_end)
tmp$month_end <- as.numeric(japan$month_end)
tmp$year_end <- as.numeric(japan$year_end)
tmp$time_end <- paste(tmp$year_end, tmp$month_end, tmp$day_end, sep = "-")
tmp$time_end <- lubridate::ymd(tmp$time_end)

tmp$capture_method <- japan$capture_method

write.csv(tmp, here::here("outputs", "JAPAN.csv"), row.names = FALSE)
