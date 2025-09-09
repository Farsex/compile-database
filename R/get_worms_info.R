#' Retrieve accepted name and classification using WoRMS API
#'
#' @param x a `character` of length 1. The name to look for?
#'
#' @export
#'
#' @examples
#' get_worms_info("Discodoris golaia")
#' get_worms_info("Odostomia japonica")

get_worms_info <- function(x) {
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

  if (length(x) != 1) {
    stop("Argument 'x' must be a character of length 1")
  }

  if (is.na(x)) {
    stop("Argument 'x' cannot be NA")
  }

  ## Query WoRMS API ----

  records <- worrms::wm_records_names(x) |>
    _[[1]] |>
    as.data.frame()

  ## Record not found ----

  if (nrow(records) == 0) {
    return(
      data.frame(
        "original_name" = x,
        "accepted_name" = NA,
        "aphia_id" = NA,
        "kingdom" = NA,
        "phylum" = NA,
        "class" = NA,
        "order" = NA
      )
    )
  }

  pos <- which(records$"status" == "accepted")

  ## Multiple accepted names ----

  if (length(pos) > 1) {
    stop("Multiple matches found")
  }

  ## Names is accepted ----

  if (length(pos) == 1) {
    records <- records[pos, ]
  }

  ## Not accepted name ----

  if (length(pos) == 0) {
    ## Different synonyms ----

    if (nrow(records) > 1) {
      stop("Multiple matches found")
      ## Get accepted name ----
    } else {
      records <- worrms::wm_records_names(records[1, "valid_name"]) |>
        _[[1]] |>
        as.data.frame()

      if (nrow(records) > 1) {
        stop("Multiple matches found")
      }
    }
  }

  data.frame(
    "original_name" = x,
    "accepted_name" = records[1, "valid_name"],
    "aphia_id" = records[1, "valid_AphiaID"],
    "kingdom" = records[1, "kingdom"],
    "phylum" = records[1, "phylum"],
    "class" = records[1, "class"],
    "order" = records[1, "order"]
  )
}
