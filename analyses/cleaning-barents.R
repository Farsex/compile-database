years <- 2004:2022

final <- data.frame()

for (year in years) {
  cat(year, "\n")

  species <- read.csv2(
    here::here(
      "data",
      "barents",
      paste0(
        "Barents Sea ecosystem survey fish diversity data export ",
        year,
        " NO"
      ),
      "input",
      "Barents Sea Fish Reference List.csv"
    )
  )

  path <- here::here(
    "data",
    "barents",
    paste0(
      "Barents Sea ecosystem survey fish diversity data export ",
      year,
      " NO"
    ),
    "output",
    "baseline",
    "StoxBiotic"
  )

  cruise <- read.delim(file.path(path, "Cruise.txt"), h = TRUE)
  haul <- read.delim(file.path(path, "Haul.txt"), h = TRUE)
  station <- read.delim(file.path(path, "Station.txt"), h = TRUE)
  individual <- read.delim(file.path(path, "Individual.txt"), h = TRUE)
  sample <- read.delim(file.path(path, "Sample.txt"), h = TRUE)
  spcat <- read.delim(file.path(path, "SpeciesCategory.txt"), h = TRUE)

  sp_code <- strsplit(individual$"SpeciesCategoryKey", "/") |>
    lapply(function(x) x[2]) |>
    unlist()

  individual$"sp_code" <- sp_code
  individual <- merge(
    individual,
    species[, c("Value", "Scientific")],
    by.x = "sp_code",
    by.y = "Value"
  )

  haul <- haul[, c("StationKey", "HaulKey", "Gear")]
  station <- station[, c(
    "StationKey",
    "BottomDepth",
    "Longitude",
    "Latitude",
    "DateTime"
  )]

  station <- merge(haul, station, by = "StationKey", all = TRUE)

  date_time <- strsplit(station$"DateTime", "[A-Z]")

  station$"date_start" <- lapply(date_time, function(x) x[1]) |> unlist()

  individual <- merge(individual, station, by = "StationKey", all = FALSE)

  # Prepare final table

  clean_data <- readxl::read_xlsx(here::here("database_template.xlsx")) |>
    as.data.frame()

  tmp <- as.data.frame(matrix(
    NA,
    ncol = ncol(clean_data),
    nrow = nrow(individual)
  ))
  colnames(tmp) <- colnames(clean_data)

  # Fill data
  tmp$"origin_dataset_name" <- "Barents Sea"
  tmp$"reference_id" <- paste(
    individual$"CruiseKey",
    individual$"StationKey",
    individual$"HaulKey.x",
    sep = "-"
  )

  tmp$"original_row_identifier" <- individual$"Individual"
  tmp$"original_binomial_name" <- individual$"Scientific"

  pos <- which(is.na(individual$"IndividualSex"))
  if (length(pos) > 0) {
    tmp[pos, "n_total"] <- 1
  }

  pos <- which(individual$"IndividualSex" == "F")
  if (length(pos) > 0) {
    tmp[pos, "number_female"] <- 1
  }

  pos <- which(individual$"IndividualSex" == "M")
  if (length(pos) > 0) {
    tmp[pos, "number_male"] <- 1
  }

  tmp$"biological_scale" <- "Individual"

  tmp$"original_age" <- individual$"IndividualAge"
  # tmp$"original_age_unit"

  tmp$"original_body_size" <- individual$"IndividualTotalLength"
  tmp$"original_body_size_type" <- "Total length"
  # tmp$"original_body_size_unit"

  tmp$"original_body_mass" <- individual$"IndividualRoundWeight"
  tmp$"original_body_mass_type" <- individual$"WeightMeasurement"
  # tmp$"original_body_mass_unit"

  tmp$"latitude_start" <- individual$"Latitude"
  tmp$"longitude_start" <- individual$"Longitude"
  tmp$"asl" <- individual$"BottomDepth"

  tmp$"date_start" <- individual$"date_start"
  tmp$"year_start" <- substr(individual$"date_start", 1, 4)
  tmp$"month_start" <- substr(individual$"date_start", 6, 7)
  tmp$"day_start" <- substr(individual$"date_start", 9, 10)
  tmp$"capture_method" <- individual$"Gear"

  final <- rbind(final, tmp)
}


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


tmp <- as.data.frame(matrix(NA, ncol = length(cols), nrow = nrow(final)))
colnames(tmp) <- cols

tmp$database <- "Barents Sea"
tmp$reference_id <- final$reference_id
tmp$original_row_identifier <- final$original_row_identifier

tmp$original_binomial_name <- final$original_binomial_name
tmp$number_female <- final$number_female
tmp$number_male <- final$number_male
tmp$n_total <- final$n_total
tmp$biological_scale <- "Individual"

tmp$original_body_size <- final$original_body_size
tmp$original_body_size_type <- final$original_body_size_type
tmp$original_body_size_unit <- final$original_body_size_unit

tmp$original_body_mass <- final$original_body_mass
tmp$original_body_mass_type <- final$original_body_mass_type
tmp$original_body_mass_unit <- final$original_body_mass_unit

tmp$asl <- -1 * final$asl

tmp$longitude_start <- final$longitude_start
tmp$latitude_start <- final$latitude_start
tmp$longitude_end <- final$longitude_end
tmp$latitude_end <- final$latitude_end

tmp$day_start <- as.numeric(final$day_start)
tmp$month_start <- as.numeric(final$month_start)
tmp$year_start <- as.numeric(final$year_start)
tmp$time_start <- final$date_start

tmp$capture_method <- final$capture_method

write.csv(tmp, here::here("outputs", "BARENTS.csv"), row.names = FALSE)
