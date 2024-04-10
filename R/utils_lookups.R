#' ICS lookup table for health and ONS codes to names
#'
#' @description provides table of ics codes and names
#'
#' @return tibble with three columns, ICB22CD, ICB22CDH, ICB22NM
#' @importFrom readxl read_excel
#' @importFrom dplyr distinct
#' @noRd
lookup_ics_table <- function() {
  filepath <- "data-raw/icb_region_lkp.xlsx"

  if (!file.exists(filepath)) {
    download.file(
      url = "https://www.arcgis.com/sharing/rest/content/items/68981b71bc144e62bf4f11045f5d4ad4/data",
      destfile = filepath,
      mode = "wb"
    )

  }

  ics22_lkp_tbl <- readxl::read_excel(
    filepath,
    sheet = "LOC22_ICB22_NHSER22_EN_LU") |>
    dplyr::distinct(
      ICB22CD,
      ICB22CDH,
      ICB22NM,
      NHSER22NM
    )

  return(ics22_lkp_tbl)
}


#' vector of ICS names
#'
#' @description provides character vector of ICS names in alphabetical order
#'
#' @return character vector
#' @importFrom dplyr pull
#' @noRd
lookup_ics_names <- function(with_regions = FALSE) {
  ics_nms <- lookup_ics_table()

  if (with_regions) {
    ics_nms <- ics_nms |>
      select("NHSER22NM", "ICB22NM") |>
      arrange(
        !!sym("NHSER22NM"),
        !!sym("ICB22NM")
      ) |>
      rev() |>
      unstack() |>
      as.list()
  } else {
    ics_nms <- ics_nms |>
      dplyr::pull(ICB22NM) |>
      sort()
  }


  return(ics_nms)
}


#' ICS22 health code from name
#'
#' @param ics_name string; ICB name beginning with "NHS"
#'
#' @return single item character vector beginning with "Q" and three letters
#'   long
#'
#' @importFrom dplyr pull filter
#'
#' @noRd
ics_code_lkp <- function(ics_name) {
  ics_code <- lookup_ics_table() |>
    filter(ICB22NM == ics_name) |>
    pull(ICB22CDH)
  return(ics_code)
}

#' ICS22 name from health code
#'
#' @param ics_code string; three letters, beginning with "Q"
#'
#' @return single item character vector
#'
#' @importFrom dplyr pull filter
#'
#' @noRd
ics_name_lkp <- function(ics_code) {
  ics_name <- lookup_ics_table() |>
    filter(ICB22CDH == ics_code) |>
    pull(ICB22NM)
  return(ics_name)
}
