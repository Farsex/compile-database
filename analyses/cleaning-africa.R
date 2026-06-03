# Donnees Afrique IRD

x <- readr::read_csv2(
  here::here(
    "data",
    "Afrique",
    "Dataset Northwest IRD",
    "25-01-16-15-14_extraction_biologie.csv"
  )
) |>
  as.data.frame()

clean_data <- readxl::read_xlsx(here::here("database_template.xlsx")) |>
  as.data.frame()


tmp <- as.data.frame(matrix(NA, ncol = ncol(clean_data), nrow = nrow(x)))
colnames(tmp) <- colnames(clean_data)

tmp$"origin_dataset_name" <- "PPEAO"
tmp$"reference_id" <- paste(x$"Station", x$"Site", x$"Campagne", sep = "-")

tmp$"original_binomial_name" <- x$"Espece"

pos <- which(is.na(x$"Sexe_id"))
if (length(pos) > 0) {
  tmp[pos, "n_total"] <- 1
}

pos <- which(x$"Sexe_id" == "i")
if (length(pos) > 0) {
  tmp[pos, "n_total"] <- 1
}

pos <- which(x$"Sexe_id" == "f")
if (length(pos) > 0) {
  tmp[pos, "number_female"] <- 1
}

pos <- which(x$"Sexe_id" == "m")
if (length(pos) > 0) {
  tmp[pos, "number_male"] <- 1
}

tmp$"biological_scale" <- "Individual"

tmp$"date_start" <- as.character(lubridate::dmy(x$"Date_coup_peche"))
tmp$"year_start" <- substr(tmp$"date_start", 1, 4)
tmp$"month_start" <- substr(tmp$"date_start", 6, 7)
tmp$"day_start" <- substr(tmp$"date_start", 9, 10)

tmp$"location" <- paste(x$"Pays", x$"Systeme", sep = " | ")
tmp$"asl" <- x$"Profondeur"

tmp$"temperature" <- x$"Temperature_fond"
tmp$"salinity" <- x$"Salinite_fond"

# capture_method
tmp$longitude_start <- parzer::parse_lon(x$"Station_longitude")
tmp$latitude_start <- parzer::parse_lat(x <- x$"Station_latitude")


# original_life_stage

tmp$"maturity_stage" <- x$"Stade_maturite"
# tmp$"maturity_stage_scale"

# original_age
# original_age_unit

tmp$"original_body_size" <- x$"Longueur_fourche"
tmp$"original_body_size_type" <- "Longueur_fourche"
# tmp$original_body_size_unit

tmp$"original_body_mass" <- x$"Poids_individu"
# original_body_mass_type
# original_body_mass_unit
