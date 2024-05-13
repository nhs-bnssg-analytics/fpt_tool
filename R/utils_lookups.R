#' ICS lookup table for health and ONS codes to names
#'
#' @description provides table of ics codes and names
#'
#' @return tibble with three columns, ICB22CD, ICB22CDH, ICB22NM
#' @importFrom readxl read_excel
#' @importFrom dplyr distinct
#' @importFrom here here
#' @noRd
lookup_ics_table <- function() {
  filepath <- here::here("data-raw/icb_region_lkp.xlsx")

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

  if (with_regions == TRUE) {
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

trust_ics_proportions <- function() {
  #lsoa code to icb code
  lsoa_to_icb <- readxl::read_excel(
    path = "C:/Users/Sebastian.Fox/Documents/R/Play/d_and_c/data-raw/Lookups/lsoa_icb.xlsx",
    sheet = "LSOA11_LOC22_ICB22_LAD22"
  ) |>
    distinct(
      LSOA11CD,
      ICB22CDH
    )

  # now create MSOA to ICB, with a count of ICBs in an MSOA (eg, if an MSOA goes
  # over an ICB boundary, then it will allow us to divide the final metric by 2)
  msoa_to_icb <- read.csv("C:/Users/Sebastian.Fox/Documents/R/Play/d_and_c/data-raw/Lookups/lsoa_to_msoa.csv") |>
    distinct(
      LSOA11CD, MSOA11CD
    ) |>
    left_join(
      lsoa_to_icb,
      by = join_by(LSOA11CD)
    ) |>
    distinct(
      MSOA11CD, ICB22CDH
    ) |>
    dplyr::add_count(
      MSOA11CD,
      name = "divisor"
    )

  latest_proportions <- readxl::read_excel(
    "C:/Users/Sebastian.Fox/Documents/R/Play/d_and_c/data-raw/Catchment populations/catchment-populations.xlsx",
    sheet = "All Admissions"
  ) |>
    filter(
      CatchmentYear == max(CatchmentYear)
    ) |>
    left_join(
      msoa_to_icb,
      by = join_by(
        msoa == MSOA11CD
      ),
      relationship = "many-to-many"
    ) |>
    summarise(
      patients = sum(patients / divisor),
      .by = c(TrustCode, TrustName, ICB22CDH)
    ) |>
    mutate(
      proportion = patients / sum(patients),
      .by = ICB22CDH,
      .keep = "unused"
    )

  return(latest_proportions)
}


#' Load model object, or part of model object
#'
#' @param type character(1); either "wf" or "perm_imp"
#'
#' @noRd
load_model_object <- function(type = NULL) {

  performance_metrics <- performance_metrics()

  model <- readRDS("C:/Users/Sebastian.Fox/Documents/R/Play/d_and_c/outputs/model_objects/wfs_log_pi.rds")[performance_metrics]

  if (!is.null(type)) {
    type <- match.arg(
      type,
      c("wf", "perm_imp")
    )

    model <- model |>
      lapply(
        \(x) x[[type]]
      )
  }

  return(model)
}
