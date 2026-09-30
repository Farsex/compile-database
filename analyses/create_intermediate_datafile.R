fields <- readxl::read_xlsx(here::here("FARsex_metadata_2026-09-29.xlsx")) |>
  as.data.frame()
fields <- fields[, "Column name", drop = TRUE]

files <- list.files(here::here("outputs"), full.names = TRUE)

i <- 2
data <- read.csv(files[i])
dbname <- "Argentina"


newdata <- data.frame(matrix(nrow = 0, ncol = length(fields)))
colnames(newdata) <- fields

keys <- paste(
  data$reference_id,
  data$original_binomial_name,
  data$original_life_stage,
  sep = " __ "
)

unique_keys <- keys |>
  unique()

aggregated_data <- lapply(unique_keys, function(x) {
  subdata <- data[keys == x, ]

  data.frame(
    farsex_row_id = NA,
    original_data_source = dbname,
    compiled_dataset_name = "FARsex-compiled-data-Fish",
    comment_in_compiled_dataset = "no",
    original_binomial_name = unique(subdata$"original_binomial_name"),
    farsex_binomial_name = NA,
    taxonomic_reference = "WORMS",
    ott_id = NA,
    class = NA,
    order = NA,
    family = NA,
    genus = NA,
    species = NA,
    number_female = sum(subdata$number_female, na.rm = TRUE),
    number_male = sum(subdata$number_male, na.rm = TRUE),
    n_total = NA,
    proportion_of_males = NA,
    sexing_method = NA,
    life_stage = unique(subdata$"original_life_stage"),
    latitude = unique(subdata$"latitude_start"),
    longitude = unique(subdata$"longitude_start"),
    location = NA,
    elevation_relative_to_sea_level = unique(subdata$"asl"),
    month_start = NA,
    month_end = NA,
    year_start = unique(subdata$"year_start"),
    year_end = NA,
    season = NA,
    body_length_type = "Unknown",
    body_length_male_mm = round(
      10 * mean(subdata[!is.na(subdata$"number_male"), "original_body_size"])
    ),
    body_length_female_mm = round(
      10 * mean(subdata[!is.na(subdata$"number_female"), "original_body_size"])
    ),
    body_mass_male_g = NA,
    body_mass_female_g = NA
  )
})

aggregated_data <- do.call(rbind.data.frame, aggregated_data)

## Compute total number

aggregated_data$"n_total" <- aggregated_data$"number_female" +
  aggregated_data$"number_male"


## Remove rows w/ no individual

aggregated_data <- aggregated_data[aggregated_data$"n_total" != 0, ]


## Compute SR

aggregated_data$"proportion_of_males" <- round(
  aggregated_data$"number_male" / aggregated_data$"n_total",
  2
)


## Check columns

check_missing_values(aggregated_data)
check_coords(aggregated_data)
check_positive_numbers(aggregated_data)


## Retrieve taxonomy

species_names <- unique(aggregated_data$"original_binomial_name") |> sort()

species_names <- gsub(
  "^Xystreuris rasile$",
  "Xystreurys rasilis",
  species_names
)


taxo <- lapply(species_names, get_worms_info)
taxo <- do.call(rbind.data.frame, taxo)

splitter <- strsplit(taxo$"accepted_name", " ")
taxo$genus <- unlist(lapply(splitter, function(x) x[1]))
taxo$species <- unlist(lapply(splitter, function(x) x[2]))

## OTT

taxo$ott_id <- rotl::tnrs_match_names(taxo$accepted_name)$"ott_id"

for (i in 1:nrow(taxo)) {
  pos <- which(aggregated_data$original_binomial_name == taxo[i, "original_name"])

  if (length(pos) > 0) {
    aggregated_data[pos, "farsex_binomial_name"] <- taxo[i, "accepted_name"]
    aggregated_data[pos, "class"] <- taxo[i, "class"]
    aggregated_data[pos, "order"] <- taxo[i, "order"]
    aggregated_data[pos, "family"] <- taxo[i, "family"]
    aggregated_data[pos, "genus"] <- taxo[i, "genus"]
    aggregated_data[pos, "species"] <- taxo[i, "species"]
    aggregated_data[pos, "ott_id"] <- taxo[i, "ott_id"]
  }
}

aggregated_data <- aggregated_data[, colnames(aggregated_data) != "original_binomial_name"]



## Merge all fish datasets !!!

## Order data

## FARSEX ID

aggregated_data$"farsex_row_id" <- sprintf("F%08d", seq_len(nrow(aggregated_data)))

anyDuplicated(aggregated_data$"farsex_row_id")
