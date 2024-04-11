#' provides a table of demand and capacity metrics with their values that the
#' user can adjust
#'
#' @description A table of data for future years based on scenarios
#' @param ics_code string; three letter health code for ICS (beginning with "Q")
#' @param horizon integer; number of years to project forward
#' @param scenario string; selected scenario to apply to the data
#'   ("last_known_year", "percent_change", "linear")
#' @param percent numeric value; a percentage change to apply to each year from
#'   the latest known year
#' @param linear_years integer; the number of years to exptrapolate a linear
#'   trend from for each metric
#' @return A tibble, where the first column is the input type (demand or
#'   capacity), the second column is the metric name, and the remaining columns
#'   are the future year values based on the selected scenario
#' @importFrom dplyr group_by select arrange ungroup mutate
#' @importFrom tidyr complete fill pivot_wider nesting
#' @importFrom rlang sym
#' @noRd
scenario_inputs <- function(ics_code, horizon, scenario,
                            percent = NULL, linear_years = NULL) {
  scenario <- match.arg(
    scenario,
    c("last_known_year",
      "percent_change",
      "linear")
  )

  if (scenario == "percent_change" &
      is.null(percent))
    stop("percent must not be missing when percent_change is applied")

  if (scenario == "linear" &
      is.null(linear_years))
    stop("linear_years must not be missing when linear is applied")

  historic_data <- ics_data(
    ics_code = ics_code,
    domain = c("Demand", "Capacity")
  ) |>
    select(
      "domain", "metric", "year", "value"
    )

  earliest_end_year <- historic_data |>
    filter(
      !!sym("year") == max(!!sym("year")),
      .by = !!sym("metric")
    ) |>
    distinct(!!sym("year")) |>
    filter(!!sym("year") == min(!!sym("year"))) |>
    pull(!!sym("year"))

  if (scenario == "last_known_year") {

    long_metric_data <- historic_data |>
      filter(
        !!sym("year") >= earliest_end_year
      ) |>
      tidyr::complete(
        tidyr::nesting(
          !!sym("metric"),
          !!sym("domain")
        ),
        year = seq(
          from = earliest_end_year,
          to = max(!!sym("year")) + horizon,
          by = 1
        )
      ) |>
      dplyr::group_by(
        !!sym("metric"),
        !!sym("domain")) |>
      tidyr::fill(
        !!sym("value"),
        .direction = "down"
      ) |>
      dplyr::ungroup()
  } else if (scenario == "percent_change") {

    percent <- 1 + (percent / 100)

    long_metric_data <- historic_data |>
      filter(
        !!sym("year") >= earliest_end_year
      ) |>
      tidyr::complete(
        tidyr::nesting(
          !!sym("metric"),
          !!sym("domain")
        ),
        year = seq(
          from = earliest_end_year,
          to = max(!!sym("year")) + horizon,
          by = 1
        )
      ) |>
      dplyr::group_by(
        !!sym("metric"),
        !!sym("domain")) |>
      mutate(
        index = cumsum(is.na(!!sym("value")))
      ) |>
      tidyr::fill(
        !!sym("value"),
        .direction = "down"
      ) |>
      mutate(
        value = value * (percent ^ index)
      ) |>
      ungroup() |>
      select(!c("index"))
  }

  wide_metric_data <- long_metric_data |>
    arrange(
      !!sym("domain"),
      !!sym("metric"),
      !!sym("year")
    ) |>
    tidyr::pivot_wider(
      names_from = !!sym("year"),
      values_from = !!sym("value")
    )

  return(wide_metric_data)
}
