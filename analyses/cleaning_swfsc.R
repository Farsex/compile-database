## Import original data ----

cruise <- read.csv2(here::here(
  "data",
  "SWFSC",
  "FRDCPSTrawlLHHaulCatch_a152_e348_cb8e.csv"
))

colnames(cruise) <- paste(colnames(cruise), cruise[1, ], sep = "_")

cruise <- cruise[-1, ]

cruise <- cruise[, c(
  "cruise_NA",
  "haul_NA",
  "longitude_degrees_east",
  "latitude_degrees_north",
  "stop_latitude_",
  "stop_longitude_",
  "time_UTC",
  "haulback_time_UTC",
  "surface_temp_degree C"
)]

pos <- which(duplicated(paste(cruise$"cruise_NA", cruise$"haul_NA")))
cruise <- cruise[-pos, ]


specimen <- read.csv2(here::here(
  "data",
  "SWFSC",
  "FRDCPSTrawlLHSpecimen_df35_162f_f883.csv"
))

colnames(specimen) <- paste(colnames(specimen), specimen[1, ], sep = "_")

specimen <- specimen[-1, ]


specimen <- specimen[, c(
  "cruise_NA",
  "haul_NA",
  "scientific_name_",
  "specimen_number_NA",
  "sex_",
  "standard_length_mm",
  "weight_g"
)]


dat <- merge(cruise, specimen, by = c("cruise_NA", "haul_NA"))

colnames(dat) <- c(
  "reference_ID",
  "reference_ID_haul",
  "longitude_start",
  "latitude_start",
  "longitude_end",
  "latitude_end",
  "date_start",
  "date_end",
  "temperature",
  "original_binomial_name",
  "n_total",
  "sex",
  "original_body_size",
  "original_body_mass"
)

dat <- data.frame(
  dat,
  "original_body_size_type" = rep("Standard length", nrow(dat)),
  "original_body_size_unit" = rep("mm", nrow(dat)),
  "original_body_mass_type" = rep(NA, nrow(dat)),
  "original_body_mass_unit" = rep("g", nrow(dat)),
  "biological_scale" = rep("individual", nrow(dat))
)


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


tmp <- as.data.frame(matrix(NA, ncol = length(cols), nrow = nrow(dat)))
colnames(tmp) <- cols

tmp$database <- "SWFSC"
tmp$reference_id <- dat$reference_ID

tmp$original_binomial_name <- dat$original_binomial_name

tmp$n_total <- dat$n_total
tmp$biological_scale <- "Individual"

tmp$original_body_size <- dat$original_body_size
tmp$original_body_size_type <- dat$original_body_size_type
tmp$original_body_size_unit <- dat$original_body_size_unit

tmp$original_body_mass <- dat$original_body_mass
tmp$original_body_mass_type <- dat$original_body_mass_type
tmp$original_body_mass_unit <- dat$original_body_mass_unit

tmp$longitude_start <- dat$longitude_start
tmp$latitude_start <- dat$latitude_start
tmp$longitude_end <- dat$longitude_end
tmp$latitude_end <- dat$latitude_end

tmp$date_start <- gsub("T.*", "", dat$date_start)
tmp$year_start <- as.numeric(substr(tmp$year_start, 1, 4))
tmp$month_start <- as.numeric(substr(tmp$month_start, 6, 7))
tmp$day_start <- as.numeric(substr(tmp$day_start, 9, 10))

tmp$date_end <- gsub("T.*", "", dat$date_end)
tmp$year_end <- as.numeric(substr(tmp$year_end, 1, 4))
tmp$month_end <- as.numeric(substr(tmp$month_end, 6, 7))
tmp$day_end <- as.numeric(substr(tmp$day_end, 9, 10))

pos <- which(dat$"sex" == "male")
if (length(pos) > 0) {
  tmp[pos, "number_male"] <- 1
  tmp[pos, "number_female"] <- 0
}

pos <- which(dat$"sex" == "female")
if (length(pos) > 0) {
  tmp[pos, "number_female"] <- 1
  tmp[pos, "number_male"] <- 0
}

pos <- which(dat$"sex" == "unknown")
if (length(pos) > 0) {
  tmp[pos, "number_female"] <- 0
  tmp[pos, "number_male"] <- 0
}

tmp <- tmp[rep(row.names(tmp), times = tmp$n_total), ]

tmp$original_row_identifier <- seq_len(nrow(tmp))

write.csv(tmp, here::here("outputs", "SWFSC.csv"), row.names = FALSE)
