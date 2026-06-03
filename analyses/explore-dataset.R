library(ggplot2)
library(patchwork)

dat <- read.csv(here::here("outputs", "ARGENTINA.csv"))

# Number of individuals per species w/ sex

per_species <- dat |>
  dplyr::mutate(has_sex = !is.na(number_female) | !is.na(number_male)) |>
  dplyr::group_by(original_binomial_name, has_sex) |>
  dplyr::summarise(n_individuals = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::arrange(n_individuals)

# per_species$"original_binomial_name" <- factor(
#   x = per_species$"original_binomial_name",
#   levels = per_species$"original_binomial_name"
# )

with_sex <- ggplot(
  per_species,
  aes(x = original_binomial_name, y = n_individuals, fill = has_sex)
) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  labs(y = "Number of individuals") +
  coord_flip() +
  theme(axis.title.y = element_blank())

# Number of individuals per species w/ age

per_species <- dat |>
  dplyr::mutate(has_age = !is.na(original_age) | !is.na(maturity_stage)) |>
  dplyr::group_by(original_binomial_name, has_age) |>
  dplyr::summarise(n_individuals = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::arrange(n_individuals)

with_age <- ggplot(
  per_species,
  aes(x = original_binomial_name, y = n_individuals, fill = has_age)
) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  labs(y = "Number of individuals") +
  coord_flip() +
  theme(axis.title.y = element_blank())


with_sex / with_age

# Number of individuals per year

per_years <- dat |>
  dplyr::group_by(year_start) |>
  dplyr::summarise(n_individuals = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::arrange(year_start)

ggplot(per_years, aes(x = year_start, y = n_individuals)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(y = "Number of individuals") +
  theme(axis.title.x = element_blank())


# Map

data_sf <- sf::st_as_sf(
  dat[, c("longitude_start", "latitude_start", "original_binomial_name")],
  coords = c("longitude_start", "latitude_start"),
  crs = 4326
)

ne_oceans <- sf::st_read(
  here::here("data", "basemap", "ne_oceans.gpkg"),
  quiet = TRUE
)
ne_countries <- sf::st_read(
  here::here("data", "basemap", "ne_countries.gpkg"),
  quiet = TRUE
)
ne_bbox <- sf::st_read(
  here::here("data", "basemap", "ne_bbox.gpkg"),
  quiet = TRUE
)
ne_graticules <- sf::st_read(
  here::here("data", "basemap", "ne_graticules.gpkg"),
  quiet = TRUE
)

ne_poles <- ne_countries[ne_countries$admin %in% c("Greenland", "Antarctica"), ]

data_sf <- sf::st_transform(data_sf, crs = sf::st_crs(ne_bbox))

ggplot() +

  geom_sf(data = ne_bbox, fill = "#cdeafc", col = NA, linewidth = 0.75) +
  geom_sf(data = ne_graticules, col = "#bae2fb", linewidth = 0.10) +

  geom_sf(data = data_sf, color = "red") +

  geom_sf(
    data = ne_countries,
    fill = "#c0c0c0",
    col = "#c9c9c9",
    linewidth = 0.10
  ) +

  geom_sf(data = ne_poles, fill = "white", col = "white") +
  geom_sf(
    data = ne_bbox,
    fill = NA,
    col = "#a6a6a6",
    linewidth = 0.75
  ) +

  theme_void()
