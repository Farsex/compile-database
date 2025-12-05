## Parameters ----

season <- "Fall"
id <- "22560"
dbname <- paste0("nefsc_bottomtrawl_", season)


## Import original data ----

bio <- read.csv(here::here(
  "data",
  season,
  paste0(id, "_NEFSC", season, "FisheriesIndependentBottomTrawlData"),
  paste0(id, "_UNION_FSCS_SVBIO.csv")
))

bio <- bio[!is.na(bio$"SVSPP"), ]


## Create unique id ----

bio$"row_id" <- 1:nrow(bio)


## Import species names ----

cat <- read.csv(here::here(
  "data",
  season,
  paste0(id, "_NEFSC", season, "FisheriesIndependentBottomTrawlData"),
  paste0(id, "_UNION_FSCS_SVCAT.csv")
))

species <- cat[, c("SVSPP", "SCIENTIFIC_NAME")]
species <- species[!duplicated(species$"SVSPP"), ]


## Import maturity codes ----

maturity <- read.csv(here::here(
  "data",
  season,
  "SVDBS_SupportTables",
  "SVDBS_MATURITY_CODES.csv"
))


## Import station metadata ----

sta <- read.csv(here::here(
  "data",
  season,
  paste0(id, "_NEFSC", season, "FisheriesIndependentBottomTrawlData"),
  paste0(id, "_UNION_FSCS_SVSTA.csv")
))


gears <- read.csv(here::here(
  "data",
  season,
  "SVDBS_SupportTables",
  "SVDBS_SVGEAR.csv"
))


## Import final db structure ----

metadata <- readxl::read_xlsx(
  here::here(
    "data",
    "NEFSC_Bottom Trwl_metadata.xlsx"
  ),
  sheet = 1
)


## Final dataset ----

farsex <- data.frame(matrix(nrow = nrow(bio), ncol = nrow(metadata)))
colnames(farsex) <- metadata[["farsex_variable_name"]]

## IDs ----

farsex$"row_id" <- bio$"row_id"
farsex$"reference_id" <- bio$"CRUISE"
farsex$"original_row_identifier" <- bio$"ID"


## Taxonomy ----

farsex$"SVSPP" <- bio$"SVSPP"

farsex <- merge(farsex, species, by = "SVSPP", all.x = TRUE, all.y = FALSE)

farsex$"original_binomial_name" <- farsex$"SCIENTIFIC_NAME"
farsex <- farsex[, c("row_id", metadata[["farsex_variable_name"]])]

farsex <- farsex[order(farsex$"row_id"), ]


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


## Age ----

farsex$"original_age" <- bio$"AGE"
farsex$"original_age_unit" <- "years"


## Body size ----

farsex$"original_body_size" <- bio$"LENGTH"


## Body mass ----

farsex$"original_body_mass" <- bio$"INDWT"


## Biological scale ----

farsex$"biological_scale" <- "individual"


## Maturity ----

farsex$"MATURITY" <- bio$"MATURITY"

farsex[which(!(farsex$"MATURITY" %in% maturity$"maturity")), "MATURITY"] <- NA

for (i in 1:nrow(maturity)) {
  pos <- which(farsex$"MATURITY" == maturity[i, "maturity"])
  if (length(pos) > 0) {
    farsex[pos, "maturity_stage"] <- maturity[i, "maturity_description"]
  }
}

farsex <- farsex[, c("row_id", metadata[["farsex_variable_name"]])]


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
farsex$"asl" <- farsex$"AVGDEPTH"
farsex$"date_start" <- farsex$"BEGIN_GMT_TOWDATE"
farsex$"temperature" <- farsex$"BOTTEMP"
farsex$"salinity" <- farsex$"BOTSALIN"

farsex_gears <- unique(farsex$"SVGEAR")
farsex_gears <- farsex_gears[!is.na(farsex_gears)]

for (i in 1:length(farsex_gears)) {
  pos <- which(gears$"svgear" == farsex_gears[i])
  sop <- which(farsex$"SVGEAR" == farsex_gears[i])
  farsex[sop, "capture_method"] <- gears[pos, "gear_definition"]
}


## Final clean ----

farsex <- farsex[, metadata[["farsex_variable_name"]]]

farsex$"database" <- tolower(dbname)
farsex <- farsex[, c("database", metadata[["farsex_variable_name"]])]

farsex$"original_binomial_name" <- clean_binomial_name(
  farsex$"original_binomial_name"
)

farsex <- filter_missing_data(farsex)

writexl::write_xlsx(
  farsex,
  here::here("outputs", paste0(tolower(dbname), ".xlsx"))
)


## Get accepted names ----

splist <- sort(unique(farsex$"original_binomial_name"))

taxo <- lapply(splist, get_worms_info) |> 
  do.call(rbind.data.frame, args = _)

writexl::write_xlsx(
  taxo,
  here::here("outputs", paste0(tolower(dbname), "_specieslist.xlsx"))
)


## Temporal coverage by species ----

plot_species_year(farsex)
