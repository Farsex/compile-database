## Parameters ----

seasons <- c("Fall", "Spring", "Summer", "Winter")
ids <- c("22560", "22561", "22562", "22563")

for (fff in seq_len(length(seasons))) {
  season <- seasons[fff]
  id <- ids[fff]
  dbname <- paste0("NEFSC-BOTTOMTRAWL-", season)

  ## Import original data ----

  bio <- read.csv(here::here(
    "data",
    "NEFSC",
    season,
    paste0(id, "_NEFSC", season, "FisheriesIndependentBottomTrawlData"),
    paste0(id, "_UNION_FSCS_SVBIO.csv")
  ))

  bio <- bio[!is.na(bio$"SVSPP"), ]

  ## Create unique id ----

  bio$"row_id" <- seq_len(nrow(bio))

  ## Import species names ----

  cat <- read.csv(here::here(
    "data",
    "NEFSC",
    season,
    paste0(id, "_NEFSC", season, "FisheriesIndependentBottomTrawlData"),
    paste0(id, "_UNION_FSCS_SVCAT.csv")
  ))

  species <- cat[, c("SVSPP", "SCIENTIFIC_NAME")]
  species <- species[!duplicated(species$"SVSPP"), ]

  ## Import maturity codes ----

  maturity <- read.csv(here::here(
    "data",
    "NEFSC",
    season,
    "SVDBS_SupportTables",
    "SVDBS_MATURITY_CODES.csv"
  ))

  ## Import station metadata ----

  sta <- read.csv(here::here(
    "data",
    "NEFSC",
    season,
    paste0(id, "_NEFSC", season, "FisheriesIndependentBottomTrawlData"),
    paste0(id, "_UNION_FSCS_SVSTA.csv")
  ))

  gears <- read.csv(here::here(
    "data",
    "NEFSC",
    season,
    "SVDBS_SupportTables",
    "SVDBS_SVGEAR.csv"
  ))

  bio <- merge(bio, species, by = "SVSPP", all.x = TRUE, all.y = FALSE)

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

  farsex <- as.data.frame(matrix(NA, ncol = length(cols), nrow = nrow(bio)))
  colnames(farsex) <- cols

  ## IDs ----

  farsex$database <- paste("NEFSC Bottomtrawl", season)

  farsex$"reference_id" <- bio$"CRUISE"
  farsex$"original_row_identifier" <- bio$"ID"

  farsex$"original_binomial_name" <- bio$"SCIENTIFIC_NAME"
  farsex$"original_binomial_name" <- clean_binomial_name(
    farsex$"original_binomial_name"
  )

  ## Sex ----

  farsex[, "n_total"] <- 1

  pos <- which(bio$"SEX" %in% c("0"))
  if (length(pos) > 0) {
    farsex[pos, "number_female"] <- 0
    farsex[pos, "number_male"] <- 0
  }

  pos <- which(bio$"SEX" %in% c("2", "3", "4", "5", "6", "7", "f", "F"))
  if (length(pos) > 0) {
    farsex[pos, "number_female"] <- 1
    farsex[pos, "number_male"] <- 0
  }

  pos <- which(bio$"SEX" %in% c("1", "m", "M"))
  if (length(pos) > 0) {
    farsex[pos, "number_female"] <- 0
    farsex[pos, "number_male"] <- 1
  }

  farsex$"original_age" <- bio$"AGE"
  farsex$"original_age_unit" <- "years"

  farsex$"original_body_size" <- bio$"LENGTH"
  farsex$"original_body_mass" <- bio$"INDWT"
  farsex$"biological_scale" <- "Individual"

  ## Maturity ----

  bio[which(!(bio$"MATURITY" %in% maturity$"maturity")), "MATURITY"] <- NA

  for (i in seq_len(nrow(maturity))) {
    pos <- which(bio$"MATURITY" == maturity[i, "maturity"])
    if (length(pos) > 0) {
      farsex[pos, "maturity_stage"] <- maturity[i, "maturity_description"]
    }
  }

  ## Station metadata ----

  sta <- sta[, c(
    "ID",
    "DECDEG_BEGLAT",
    "DECDEG_BEGLON",
    "DECDEG_ENDLAT",
    "DECDEG_ENDLON",
    "AVGDEPTH",
    "BEGIN_GMT_TOWDATE",
    "BOTTEMP",
    "BOTSALIN",
    "SURFTEMP",
    "SURFSALIN",
    "SVGEAR"
  )]

  farsex <- merge(
    farsex,
    sta,
    by.x = "original_row_identifier",
    by.y = "ID",
    all.x = TRUE,
    all.y = FALSE
  )

  farsex$"latitude_start" <- farsex$"DECDEG_BEGLAT"
  farsex$"longitude_start" <- farsex$"DECDEG_BEGLON"
  farsex$"latitude_end" <- farsex$"DECDEG_ENDLAT"
  farsex$"longitude_end" <- farsex$"DECDEG_ENDLON"
  farsex$"asl" <- -1 * farsex$"AVGDEPTH"
  farsex$"date_start" <- farsex$"BEGIN_GMT_TOWDATE"
  farsex$"bottom_temperature" <- farsex$"BOTTEMP"
  farsex$"bottom_salinity" <- farsex$"BOTSALIN"
  farsex$"surface_temperature" <- farsex$"SURFTEMP"
  farsex$"surface_salinity" <- farsex$"SURFSALIN"

  farsex_gears <- unique(farsex$"SVGEAR")
  farsex_gears <- farsex_gears[!is.na(farsex_gears)]

  for (i in seq_len(length(farsex_gears))) {
    pos <- which(gears$"svgear" == farsex_gears[i])
    sop <- which(farsex$"SVGEAR" == farsex_gears[i])
    farsex[sop, "capture_method"] <- gears[pos, "gear_definition"]
  }

  ## Final clean ----

  farsex <- farsex[, cols]

  farsex$"date_start" <- gsub(" .*", "", farsex$"date_start")
  farsex$"date_start" <- lubridate::mdy(farsex$"date_start")

  farsex$"year_start" <- substr(farsex$"date_start", 1, 4)
  farsex$"month_start" <- substr(farsex$"date_start", 6, 7)
  farsex$"day_start" <- substr(farsex$"date_start", 9, 10)

  # farsex <- filter_missing_data(farsex)

  write.csv(
    farsex,
    here::here("outputs", paste0(dbname, ".csv")),
    row.names = FALSE
  )
}
