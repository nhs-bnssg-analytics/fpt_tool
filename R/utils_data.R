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
#' @noRd
snapshot_ics_data <- function(broad_age_bands = TRUE) {


  metrics <- metadata |>
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
#' @noRd
snapshot_metadata <- function() {
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


snapshot_model_accuracy <- function() {
  model_accuracy <- readRDS("C:/Users/Sebastian.Fox/Documents/R/Play/d_and_c/tests/model_testing/model_summary_information.rds") |>
    filter(
      `Test set value` == min(`Test set value`),
      .by = `Target variable`
    ) |>
    select(
      target_variable = "Target variable",
      mape = "Test set value"

    )

  return(model_accuracy)

}



# check custom data inputs ------------------------------------------------

check_custom_inputs <- function(database_table, custom_table) {

  check_names <- identical(
    names(database_table),
    names(custom_table)
  )

  check_metrics <- identical(
    database_table |> select(1:2),
    custom_table |> select(1:2)
  )

  if (!check_names) {
    check_response <- "Field names in file are not what are expected"
  } else if (!check_metrics) {
    check_response <- "First two columns of file are not what are expected"
  } else {
    check_response <- "pass"
  }

  return(check_response)
}
