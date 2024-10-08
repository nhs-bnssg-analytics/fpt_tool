# data import functions for internal functions ----------------------------

#' historic data for an ics imported from the d_and_c project and stored in the
#' current project
#'
#' @description tabulated historic data for all icss stored in the project,
#'   allowing for managed updates to d_and_c without knock on effect to this
#'   project
#' @return table of annual data containing numerator, denominator, value by year
#'   and metric
#' @param broad_age_bands logical; whether to use broad age bands (TRUE) or not
#'   (FALSE)
#' @importFrom purrr map_df
#' @importFrom dplyr distinct pull filter arrange inner_join join_by mutate
#' @importFrom rlang sym
#' @importFrom utils read.csv
#' @noRd
snapshot_ics_data <- function(broad_age_bands = TRUE) {


  metrics <- metadata |>
    distinct(
      !!sym("metric"),
      !!sym("domain"),
      !!sym("theme")
    )

  ics_timeseries <- list.files(
    "C:/Users/Sebastian.Fox/R/Play/d_and_c/data/",
    full.names = TRUE
  ) |>
    (\(x) x[!grepl("configuration-table", x)])() |>
    purrr::map_df(
      utils::read.csv
    ) |>
    filter(
      grepl("annual", !!sym("frequency"))
    )

  if (broad_age_bands == TRUE) {
    ics_timeseries <- replace_narrow_age_bands(ics_timeseries)

  }

  ics_timeseries <- ics_timeseries |>
    inner_join(
      metrics,
      by = join_by(!!sym("metric"))
    ) |>
    arrange(
      !!sym("metric"),
      !!sym("year"),
      !!sym("org")
    ) |>
    select(
      "year",
      "org",
      "domain",
      "theme",
      "metric",
      "numerator",
      "denominator",
      "value"
    )

  return(ics_timeseries)
}


#' metadata for the metrics used in modelling
#'
#' @description obtain metadata information for the metrics
#'
#' @return table of metric, domain, numerator and denominator descriptions
#' @importFrom dplyr filter select
#' @importFrom rlang sym
#' @importFrom utils read.csv
#' @noRd
snapshot_metadata <- function() {
  meta <- utils::read.csv(
    "C:/Users/Sebastian.Fox/R/Play/d_and_c/data/configuration-table.csv",
    encoding = "latin1") |>
    dplyr::filter(
      !!sym("status") == "include" |
        (!!sym("domain") == "Performance" &
           !!sym("status") == "modelled")
    ) |>
    dplyr::select(
      "metric",
      "domain",
      "theme",
      "numerator_description",
      "denominator_description"
    )

  return(meta)
}

#' @importFrom rlang sym
snapshot_model_accuracy <- function() {
  model_accuracy <- readRDS("C:/Users/Sebastian.Fox/R/Play/d_and_c/tests/model_testing/model_summary_information.rds") |>
    filter(
      `Model type` == "logistic_regression",
      `Number lagged target years` == "0 lagged years"
    ) |>
    filter(
      !!sym("Test set value") == min(!!sym("Test set value")),
      .by = !!sym("Target variable")
    ) |>
    select(
      target_variable = "Target variable",
      mape = "Test set value"

    )

  return(model_accuracy)

}

snapshot_model <- function() {
  performance_metrics <- performance_metrics()

  # model <- readRDS("C:/Users/Sebastian.Fox/R/Play/d_and_c/outputs/model_objects/wfs_best_no_lag_target_proportion_pi.rds")[performance_metrics]
  model <- readRDS("C:/Users/Sebastian.Fox/R/Play/d_and_c/outputs/model_objects/live_models.rds")[performance_metrics]

  return(model)
}

#' @importFrom utils download.file read.csv
#' @importFrom dplyr distinct
snapshot_ics_lookup <- function() {
  temp <- tempfile(fileext = "csv")

  utils::download.file(
    url = "https://opendata.arcgis.com/api/v3/datasets/2bca16d4f8e4426d80137213fce90bbd_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1",
    destfile = temp,
    mode = "wb"
  )

  ics22_lkp_tbl <- utils::read.csv(
    temp
  ) |>
    dplyr::distinct(
      !!sym("ICB22CD"),
      !!sym("ICB22CDH"),
      !!sym("ICB22NM"),
      !!sym("NHSER22NM")
    )

  return(ics22_lkp_tbl)
}

#' @importFrom rlang sym
snapshot_trust_ics_proportions <- function() {
  #lsoa code to icb code
  lsoa_to_icb <- readxl::read_excel(
    path = "C:/Users/Sebastian.Fox/R/Play/d_and_c/data-raw/Lookups/lsoa_icb.xlsx",
    sheet = "LSOA11_LOC22_ICB22_LAD22"
  ) |>
    distinct(
      !!sym("LSOA11CD"),
      !!sym("ICB22CDH")
    )

  # now create MSOA to ICB, with a count of ICBs in an MSOA (eg, if an MSOA goes
  # over an ICB boundary, then it will allow us to divide the final metric by 2)
  msoa_to_icb <- read.csv("C:/Users/Sebastian.Fox/R/Play/d_and_c/data-raw/Lookups/lsoa_to_msoa.csv") |>
    distinct(
      !!sym("LSOA11CD"), !!sym("MSOA11CD")
    ) |>
    left_join(
      lsoa_to_icb,
      by = join_by(!!sym("LSOA11CD"))
    ) |>
    distinct(
      !!sym("MSOA11CD"), !!sym("ICB22CDH")
    ) |>
    dplyr::add_count(
      !!sym("MSOA11CD"),
      name = "divisor"
    )

  latest_proportions <- readxl::read_excel(
    "C:/Users/Sebastian.Fox/R/Play/d_and_c/data-raw/Catchment populations/catchment-populations.xlsx",
    sheet = "All Admissions"
  ) |>
    filter(
      !!sym("CatchmentYear") == max(!!sym("CatchmentYear"))
    ) |>
    left_join(
      msoa_to_icb,
      by = join_by(
        !!sym("msoa") == !!sym("MSOA11CD")
      ),
      relationship = "many-to-many"
    ) |>
    summarise(
      patients = sum(!!sym("patients") / !!sym("divisor")),
      .by = c(!!sym("TrustCode"), !!sym("TrustName"), !!sym("ICB22CDH"))
    ) |>
    mutate(
      proportion = !!sym("patients") / sum(!!sym("patients")),
      .by = !!sym("ICB22CDH"),
      .keep = "unused"
    )

  return(latest_proportions)
}

