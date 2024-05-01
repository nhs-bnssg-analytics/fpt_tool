#' metadata for the metrics used in modelling
#'
#' @description obtain metadata information for the metrics
#'
#' @return table of metric, domain, numerator and denominator descriptions
#' @importFrom dplyr filter select
#' @importFrom rlang sym
#' @noRd
metadata <- function() {
  meta <- read.csv(
    "C:/Users/Sebastian.Fox/Documents/R/Play/d_and_c/data/configuration-table.csv",
    encoding = "latin1") |>
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

  return(metrics)
}

#' historic data for an ics
#'
#' @description tabulated historic data for ics of interest
#' @return table of annual data containing numerator, denominator, value by year
#'   and metric
#' @param ics_code string; health code for ics of interest
#' @param domain_type combination of "Performance", "Demand" or "Capacity"
#' @param broad_age_bands logical; whether to use broad age bands (TRUE) or not
#'   (FALSE)
#' @importFrom purrr map_df
#' @importFrom dplyr distinct pull filter arrange inner_join join_by mutate
#' @importFrom rlang sym
#' @noRd
ics_data <- function(ics_code, domain_type = NULL, broad_age_bands = TRUE) {

  if (!is.null(domain_type)) {
    if(!any(domain_type %in% c("Performance", "Demand", "Capacity"))) {
      stop("domain_type needs to be one of 'Performance', 'Demand' or 'Capacity'")
    }
  }


  metrics <- metadata()

  if (!is.null(domain_type)) {
    metrics <- metrics |>
      filter(
        !!sym("domain") %in% domain_type
      )
  }
  metrics <- metrics |>
    distinct(
      !!sym("metric"),
      !!sym("domain")
    )

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
      !!sym("org") %in% ics_code
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
      "metric",
      "numerator",
      "denominator",
      "value"
    ) |>
    mutate(
      value_type = "Observed"
    )

  return(ics_timeseries)
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
      value = numerator / denominator
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
