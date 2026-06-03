## Donnees Argentine

bio <- readxl::read_xlsx(
  here::here("data", "Argentina", "size_sex_10sp.xlsx"),
  sheet = 1
) |>
  as.data.frame()

bio$ref <- paste(bio$"survey", bio$"haul", bio$"year", sep = "-")

env <- readxl::read_xlsx(
  here::here("data", "Argentina", "size_sex_10sp.xlsx"),
  sheet = 3
) |>
  as.data.frame()

env$ref <- paste(env$"survey", env$"hawl", env$"year", sep = "-")

x <- merge(env, bio, by = "ref")


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


tmp <- as.data.frame(matrix(NA, ncol = length(cols), nrow = nrow(x)))
colnames(tmp) <- cols

tmp$"database" <- "ARGENTINA"
tmp$"reference_id" <- x$"ref"
tmp$"original_row_identifier" <- 1:nrow(x)

tmp$"original_binomial_name" <- x$"species"

pos <- which(x$"sex" == "male")
if (length(pos) > 0) {
  tmp[pos, "number_male"] <- 1
}

pos <- which(x$"sex" == "female")
if (length(pos) > 0) {
  tmp[pos, "number_female"] <- 1
}

pos <- which(x$"sex" == "indeterminate")
if (length(pos) > 0) {
  tmp[pos, "n_total"] <- 1
}

tmp$"biological_scale" <- "Individual"

tmp$"year_start" <- x$"year.x"

tmp$"asl" <- -1 * x$"depth (m)"
tmp$"surface_temperature" <- x$"temperature (ºC)"
tmp$"surface_salinity" <- x$"salinity"

tmp$"longitude_start" <- x$"long"
tmp$"latitude_start" <- x$"lat"

tmp$"original_body_size" <- x$"length"
tmp$"original_body_size_unit" <- "cm"
tmp$"original_life_stage" <- x$"development"

write.csv(tmp, here::here("outputs", "ARGENTINA.csv"), row.names = FALSE)
