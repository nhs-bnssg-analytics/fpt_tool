#' metadata for the metrics used in modelling
#'
#' @description obtain metadata information for the metrics
#'
#' @return table of metric, domain, numerator and denominator descriptions
#' @importFrom dplyr filter select
#' @importFrom rlang sym
#' @noRd
metadata <- function() {
  meta <- read.csv("C:/Users/Sebastian.Fox/Documents/R/Play/d_and_c/data/configuration-table.csv") |>
    dplyr::filter(
      !!sym("status") == "include" |
        (!!sym("domain") == "Performance" &
           !!sym("status") == "modelled")
    ) |>
    dplyr::select(
      "metric",
      "domain",
      "numerator_description",
      "denominator_description"
    )

  return(meta)
}

#' Obtain performance metrics
#'
#' @return vector of performance metrics
#' @importFrom rlang sym
#' @importFrom dplyr filter pull
#' @noRd
performance_metrics <- function() {
  metrics <- metadata() |>
    filter(
      !!sym("domain") == "Performance"#,
    ) |>
    pull(
      !!sym("metric")
    )
}

#' historic data for an ics
#'
#' @description tabulated historic data for ics of interest
#' @return table of annual data containing numerator, denominator, value by year
#'   and metric
#' @param ics_code string; health code for ics of interest
#' @param domain_type one of "Performance", "Demand" or "Capacity"
#' @importFrom purrr map_df
#' @importFrom dplyr distinct pull filter arrange
#' @importFrom rlang sym
#' @noRd
ics_data <- function(ics_code, domain_type = NULL) {
  # browser()
  metrics <- metadata()

  if (!is.null(domain_type)) {
    metrics <- metrics |>
      filter(
        !!sym("domain") == domain_type
      )
  }
  metrics <- metrics |>
    distinct(!!sym("metric")) |>
    pull()

  ics_timeseries <- list.files(
    "C:/Users/Sebastian.Fox/Documents/R/Play/d_and_c/data/",
    full.names = TRUE
  ) |>
    (\(x) x[!grepl("configuration-table", x)])() |>
    purrr::map_df(
      read.csv
    ) |>
    filter(
      grepl("annual", !!sym("frequency")),
      !!sym("org") == ics_code,
      !!sym("metric") %in% metrics
    ) |>
    arrange(
      !!sym("metric"),
      !!sym("year"),
      !!sym("org")
    ) |>
    select(
      "year",
      "org",
      "metric",
      "numerator",
      "denominator",
      "value"
    )

  return(ics_timeseries)
}
