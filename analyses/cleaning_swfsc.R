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


head(specimen)


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
  "original_body_size_type" = rep(NA, nrow(dat)),
  "original_body_size_unit" = rep("mm", nrow(dat)),
  "original_body_mass_type" = rep(NA, nrow(dat)),
  "original_body_mass_unit" = rep("g", nrow(dat)),
  "biological_scale" = rep("individual", nrow(dat))
)


dim(dat)
