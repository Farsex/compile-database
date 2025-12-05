japan <- data.frame()

sheets <- 2:5

for (sheet in sheets) {
  dat <- readxl::read_xlsx(
    here::here(
      "data",
      "japan",
      "Organized_data_Chickengrunt_Skipjacktuna_Splendidalfonsio_Coastflyingfish_20250905.xlsx"
    ),
    sheet = sheet
  )

  dat <- as.data.frame(dat)

  new_dat <- list()

  for (i in 2:ncol(dat)) {
    new_dat[[i - 1]] <- as.data.frame(t(dat[, i]))
  }

  new_dat <- do.call(rbind.data.frame, new_dat)

  colnames(new_dat) <- dat[, 1]

  japan <- rbind(japan, new_dat)
}

filename <- gsub("\\s", "_", tolower(unique(new_dat$Species)))

writexl::write_xlsx(new_dat, paste0(filename, ".xlsx"))

sheets <- 1:2

for (sheet in sheets) {
  dat <- readxl::read_xlsx(
    here::here(
      "data",
      "japan",
      "mackerel_organizeddata_20250905.xlsx"
    ),
    sheet = sheet
  )

  dat <- as.data.frame(dat)

  new_dat <- list()

  for (i in 2:ncol(dat)) {
    new_dat[[i - 1]] <- as.data.frame(t(dat[, i]))
  }

  new_dat <- do.call(rbind.data.frame, new_dat)

  colnames(new_dat) <- dat[, 1]

  japan <- rbind(japan, new_dat)
}


##

dat <- readxl::read_xlsx(
  here::here(
    "data",
    "japan",
    "Round herring_20250905.xlsx"
  ),
  sheet = 1
)

dat <- as.data.frame(dat)

new_dat <- list()

for (i in 2:ncol(dat)) {
  new_dat[[i - 1]] <- as.data.frame(t(dat[, i]))
}

new_dat <- do.call(rbind.data.frame, new_dat)

colnames(new_dat) <- dat[, 1]

japan <- rbind(japan, new_dat)



dat <- readxl::read_xlsx(
  here::here(
    "data",
    "japan",
    "Cobaltcap silverside_organized data_20250905.xlsx"
  ),
  sheet = 1
)

dat <- as.data.frame(dat)

new_dat <- list()

for (i in 2:ncol(dat)) {
  new_dat[[i - 1]] <- as.data.frame(t(dat[, i]))
}

new_dat <- do.call(rbind.data.frame, new_dat)

colnames(new_dat) <- dat[, 1]

japan <- rbind(japan, new_dat)


japan <- japan[japan$number_female != "2？", ]
