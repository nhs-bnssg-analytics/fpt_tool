

#' Obtain performance metrics
#'
#' @return vector of performance metrics
#' @importFrom rlang sym
#' @importFrom dplyr filter pull
#' @noRd
performance_metrics <- function() {
  metrics <- metadata |>
    filter(
      !!sym("domain") == "Performance"#,
    ) |>
    pull(
      !!sym("metric")
    )

  return(metrics)
}

#' historic data for an ics
#'
#' @description tabulated historic data for ics of interest
#' @return table of annual data containing numerator, denominator, value by year
#'   and metric
#' @param ics_code string; health code for ics of interest
#' @param domain_type combination of "Performance", "Demand" or "Capacity"
#' @importFrom purrr map_df
#' @importFrom dplyr distinct pull filter arrange inner_join join_by mutate
#' @importFrom rlang sym
#' @noRd
ics_data <- function(ics_code, domain_type = NULL) {

  if (!is.null(domain_type)) {
    if(!any(domain_type %in% c("Performance", "Demand", "Capacity"))) {
      stop("domain_type needs to be one of 'Performance', 'Demand' or 'Capacity'")
    }
  }


  ics_timeseries_filtered <- ics_timeseries |>
    filter(
      !!sym("org") %in% ics_code
    ) |>
    mutate(
      value_type = "Observed"
    )

  if (!is.null(domain_type)) {
    ics_timeseries_filtered <- ics_timeseries_filtered |>
      filter(
        !!sym("domain") %in% domain_type
      )
  }
  return(ics_timeseries_filtered)
}


#' Replace narrow age bands with broad age bands within tibble
#'
#' @param ics_timeseries tibble of data for ics, by year and metric
#'
#' @return tibble with same structure as input data but where the metrics that
#'   refer to age bands having been converted from narrow (10 year) age bands to
#'   broad
#' @importFrom dplyr filter mutate summarise any_of mutate bind_rows
#' @importFrom stringr str_replace_all
#' @noRd
replace_narrow_age_bands <- function(ics_timeseries) {
  age_band_data <- ics_timeseries |>
    filter(
      grepl("age band", metric)
    ) |>
    mutate(
      metric = str_replace_all(
        metric,
        c("0-9" = "0-29",
          "10-19" = "0-29",
          "20-29" = "0-29",
          "30-39" = "30-59",
          "40-49" = "30-59",
          "50-59" = "30-59",
          "60-69" = "60+",
          "70-79" = "60+",
          "80-89" = "60+",
          "80\\+" = "60+",
          "90\\+" = "60+")
      )
    ) |>
    summarise(
      numerator = sum(numerator),
      denominator = mean(denominator),
      .by = any_of(
        c(
          "year", "quarter", "month", "org", "frequency", "metric"
        )
      )
    ) |>
    mutate(
      value = (numerator / denominator) * 100
    )

  ics_timeseries <- ics_timeseries |>
    filter(
      !grepl("age band", metric)
    ) |>
    bind_rows(
      age_band_data
    )

  return(ics_timeseries)
}
