#' Clean species scientific names
#'
#' @param x a `character` vector. Species scientific names to clean.
#'
#' @param species_only a `logical` of length 1. If `TRUE` (default) aggregates
#'   subspecies, varieties, etc. to species level. Otherwise keeps names as it.
#'
#' @param higher_taxa a `logical` of length 1. If `TRUE` (default) keeps
#'   one-term names (e.g. genus, family, etc.). Otherwise replaces these names
#'   by `NA`.
#'
#' @return A `character` vector (same as `x` but with cleaned names).
#'
#' @export
#'
#' @examples
#' species <- c(
#'   "Gadus morhua",
#'   "Gadus morhua (Atlantic cod)",
#'   "Gadus morhua callarias",
#'   "Gadus",
#'   NA
#' )
#'
#' clean_binomial_name(species)
#' clean_binomial_name(species, species_only = FALSE)
#' clean_binomial_name(species, higher_taxa = FALSE)

clean_binomial_name <- function(x, species_only = TRUE, higher_taxa = TRUE) {
  ## Check argument ----

  if (missing(x)) {
    stop("Argument 'x' is required")
  }

  if (is.null(x)) {
    stop("Argument 'x' is required")
  }

  if (!is.character(x)) {
    stop("Argument 'x' must be a data.frame")
  }

  ## Remove non-authorized characters ----

  x <- gsub("\\(.*\\)", "", x)
  x <- gsub("[0-9]", " ", x)

  x <- gsub("-", "9", x)
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("9", "-", x)

  x <- gsub("\\s+", " ", x)
  x <- trimws(x)

  ## Clean character case ----

  x <- lapply(x, function(z) {
    if (!is.na(z)) {
      paste0(toupper(substr(z, 1, 1)), tolower(substr(z, 2, nchar(z))))
    } else {
      NA
    }
  })

  x <- unlist(x)

  ## species_only ? ----

  if (species_only) {
    x <- strsplit(x, " ")
    x <- lapply(x, function(z) {
      if (length(z) == 1) {
        z
      } else {
        paste(z[1], z[2], collapse = " ")
      }
    })

    x <- unlist(x)
  }

  ## higher_taxa ? ----

  if (!higher_taxa) {
    x <- strsplit(x, " ")
    x <- lapply(x, function(z) {
      if (length(z) == 1) {
        NA
      } else {
        paste(z, collapse = " ")
      }
    })

    x <- unlist(x)
  }

  x
}
