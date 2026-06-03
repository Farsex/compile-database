years <- 2004:2022

for (year in years) {
  full_url <- paste0(
    "https://ftp.nmdc.no/nmdc/IMR/surveytimeseries/Barents+Sea+ecosystem+survey+",
    "fish+diversity+data+export/ECO_DIV_",
    year,
    "_1.zip"
  )

  download.file(
    url = full_url,
    destfile = here::here(
      "data",
      "barents",
      paste0("ECO_DIV_", year, "_1.zip")
    ),
    mode = "wb"
  )

  unzip(
    zipfile = here::here(
      "data",
      "barents",
      paste0("ECO_DIV_", year, "_1.zip")
    ),
    exdir = here::here(
      "data",
      "barents"
    )
  )
}
