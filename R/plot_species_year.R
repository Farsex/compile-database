plot_species_year <- function(data) {
  dates <- lapply(strsplit(data$"date_start", " "), function(x) x[1]) |>
    unlist()

  dates <- lubridate::mdy(dates)

  tmp <- data.frame(species = data[["original_binomial_name"]], date = dates)
  tmp$"year" <- lubridate::year(tmp$"date")

  tmp <- tmp[!is.na(tmp$"year"), ]

  for_graph <- tmp |>
    dplyr::group_by(year, species) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::ungroup()

  for_graph <- as.data.frame(for_graph)

  ordres <- table(for_graph$species) |>
    as.data.frame()
  ordres <- ordres[order(ordres$Freq), ]
  for_graph$"species" <- factor(for_graph$"species", levels = ordres[, 1])
  for_graph$"year" <- factor(for_graph$"year")

  for_graph[which(for_graph$n < 200), "n"] <- NA

  ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = for_graph,
      mapping = ggplot2::aes(x = year, y = species, fill = n)
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_fill_continuous(na.value = "#dddddd") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 6),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
    )
}
