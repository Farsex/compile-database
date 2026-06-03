## Donnee NOAA Alaska

x <- readr::read_csv(here::here(
  "data",
  "Alaska",
  "Dataset Alaska",
  "106_farsex_project_data.csv"
)) |>
  as.data.frame()


clean_data <- readxl::read_xlsx(here::here("database_template.xlsx")) |>
  as.data.frame()


tmp <- as.data.frame(matrix(NA, ncol = ncol(clean_data), nrow = nrow(x)))
colnames(tmp) <- colnames(clean_data)

tmp$"origin_dataset_name" <- "NOAA ALASKA"
tmp$"reference_id" <- x$"SURVEY_NAME"

tmp$"original_binomial_name" <- x$"SPECIES_NAME"

pos <- which(x$"SEX" == 1)
if (length(pos) > 0) {
  tmp[pos, "number_male"] <- 1
}

pos <- which(x$"SEX" == 2)
if (length(pos) > 0) {
  tmp[pos, "number_female"] <- 1
}

pos <- which(x$"SEX" == 3)
if (length(pos) > 0) {
  tmp[pos, "n_total"] <- 1
}

tmp$"biological_scale" <- "Individual"

tmp$"date_start" <- substr(
  as.character(lubridate::ymd_hms(x$"DATE_TIME_START")),
  1,
  10
)

tmp$"year_start" <- substr(tmp$"date_start", 1, 4)
tmp$"month_start" <- substr(tmp$"date_start", 6, 7)
tmp$"day_start" <- substr(tmp$"date_start", 9, 10)

tmp$"asl" <- x$"DEPTH_M"

tmp$"temperature" <- x$"SURFACE_TEMPERATURE_C"

tmp$"longitude_start" <- x$"LONGITUDE_DD_START"
tmp$"longitude_end" <- x$"LONGITUDE_DD_END"
tmp$"latitude_start" <- x$"LATITUDE_DD_START"
tmp$"latitude_end" <- x$"LATITUDE_DD_END"

tmp$"original_body_mass" <- x$"WEIGHT"
# tmp$"original_body_mass_type"
tmp$"original_body_mass_unit" <- "g"

tmp$"original_body_size" <- x$"LENGTH"
# tmp$"original_body_size_type"
tmp$"original_body_size_unit" <- "mm"

mat <- readr::read_csv2(here::here(
  "data",
  "Alaska",
  "Dataset Alaska",
  "maturity_table.csv"
)) |>
  as.data.frame()

tabs <- sort(unique(mat$Table))

for (i in 1:length(tabs)) {
  tab <- mat[mat$Table == tabs[i], ]
  pos <- which(x$"MATURITY_TABLE" == tabs[i])

  if (length(pos) > 0) {
    for (j in 1:nrow(tab)) {
      from <- tab[j, "Code"]
      to <- tab[j, "Value"]

      x[pos, "MATURITY"] <- gsub(
        paste0("^", from, "$"),
        to,
        x[pos, "MATURITY"]
      )
    }
  }
}
