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
#' @importFrom dplyr group_by select arrange ungroup mutate bind_cols tibble case_when
#' @importFrom tidyr complete fill pivot_wider nesting nest unnest
#' @importFrom rlang sym
#' @importFrom purrr map map2
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

  end_year_range <- historic_data |>
    filter(
      !!sym("year") == max(!!sym("year")),
      .by = !!sym("metric")
    ) |>
    distinct(!!sym("year")) |>
    pull(!!sym("year")) |>
    range()

  earliest_end_year <- end_year_range[1]
  latest_end_year <- end_year_range[2]

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
        value = !!sym("value") * (percent ^ !!sym("index"))
      ) |>
      ungroup() |>
      select(!c("index"))
  } else if (scenario == "linear") {

    long_metric_data <- historic_data |>
      filter(
        !!sym("year") >= max(!!sym("year")) - (linear_years - 1),
        .by = c(
          !!sym("metric"),
          !!sym("domain")
        )
      ) |>
      group_by(
        !!sym("metric"),
        !!sym("domain")
      ) |>
      tidyr::complete(
        year = seq(
          from = min(!!sym("year")),
          to = latest_end_year + horizon,
          by = 1
        )
      ) |>
      tidyr::nest(
        data = c(
          !!sym("year"),
          !!sym("value")
        )
      ) |>
      mutate(
        fit = purrr::map(
          .x = data,
          .f = ~ lm(!!sym("value") ~ !!sym("year"), data = .x, na.action = na.omit)
        ),
        data = purrr::map2(
          .x = !!sym("fit"),
          .y = !!sym("data"),
          .f = ~ bind_cols(.y, tibble(prediction = predict(.x, newdata = .y)))
        ),
        data = purrr::map(
          .x = !!sym("data"),
          .f = ~ mutate(
            .x,
            value = case_when(
              is.na(!!sym("value")) ~ !!sym("prediction"),
              .default = !!sym("value")
            )
          )
        )
      ) |>
      unnest(!!sym("data")) |>
      select(!c("fit", "prediction")) |>
      filter(
        !!sym("year") >= earliest_end_year
      )

  }

  wide_metric_data <- long_metric_data |>
    ungroup() |>
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


reset_scenarios <- function(ics_cd, horizon, percent, linear_years) {
  # browser()
  last_known <- scenario_inputs(
    ics_code = ics_cd,
   # ics_code = "QUY",
    horizon = horizon,
    scenario = "last_known_year"
  )

  custom <- last_known

  percent <- scenario_inputs(
    ics_code = ics_cd,
    horizon = horizon,
    scenario = "percent_change",
    percent = percent
  )

  linear <- scenario_inputs(
    ics_code = ics_cd,
    horizon = horizon,
    scenario = "linear",
    linear_years = linear_years
  )

  output <- list(
    "last_known" = last_known,
    "percent" = percent,
    "linear" = linear,
    "custom" = custom
  )

  return(output)

}

update_predictions <- function(prediction_custom_scenario, model_outputs, r) {
  observed_data <- r$ics_data |>
    distinct(
      !!sym("year"),
      !!sym("metric")
    )

  r$predictions <- setNames(
    c("last_known", "percent", "linear", "custom"),
    nm = c(
      "Prediction - last known value",
      "Prediction - percent change",
      "Prediction - linear extrapolation",
      paste0("Prediction - ", prediction_custom_scenario)
    )
  )|>
    purrr::map_df(
      ~ model_scenario_data(
        scenario_data = r$scenario_data[[.x]],
        ics_code = r$ics_cd,
        model = model_outputs
      ),
      .id = "value_type"
    ) |>
    anti_join(
      observed_data,
      by = join_by(
        !!sym("year"),
        !!sym("metric")
      )
    )
}


#' Prioritise predictor variables based on the predictors selected
#'
#' @param model_permutation_importance a list object containing the permutation
#'   importance tables for each model. The list items should all have the name
#'   of the performance variable that is being modelled. The tables within each
#'   item should have three columns: Variable, Importance, StDev
#' @param performance_metrics character vector of the performance metrics
#'   selected. These are used to order the variables in the returned table to
#'   display which have highest importance
#' @param top_n integer(1); used to filter the returned table to the number of
#'   variable specified
#'
#' @details for some models, the important metrics could be lagged versions of
#'   the variables rather than the variable itself. To prioritise the order of
#'   the metrics to return, for each performance metric, the maximum permutation
#'   importance value is calculated for each predictor variable, where a lagged
#'   version of a predictor variable is considered alongside the unlagged
#'   version. Subsequently, the mean of the maximum values is taken across all
#'   of the models, and then sorted to present the most important variables
#' @return character vector of the variables that are most important to all of
#'   the models selected, where the first variable is the most important
#'
#' @importFrom dplyr bind_rows filter mutate summarise
#' @importFrom rlang sym
#'
#' @noRd
important_variables <- function(model_permutation_importance,
                                performance_metrics, top_n = NULL) {

  important_metrics <- model_permutation_importance |>
    bind_rows(
      .id = "metric"
    ) |>
    filter(
      !!sym("metric") %in% performance_metrics
    ) |>
    mutate(
      Variable = gsub("lag_1_|lag_2_", "", !!sym("Variable"))
    ) |>
    filter(
      !!sym("metric") != !!sym("Variable")
    ) |>
    summarise(
      Importance = max(!!sym("Importance")),
      .by = c(
        !!sym("metric"),
        !!sym("Variable")
      )
    ) |>
    summarise(
      Importance = mean(!!sym("Importance")),
      .by = c(
        !!sym("Variable")
      )
    )

  if (!is.null(top_n)) {
    important_metrics <- important_metrics |>
      head(top_n)
  }

  important_metrics <- important_metrics |>
    pull(!!sym("Variable"))

  return(important_metrics)
}

create_scenario_table <- function(custom_table, important_vars, table_type) {
  table_type <- match.arg(
    table_type,
    c("display", "stored")
  )

  if (table_type == "display") {
    output_table <- custom_table |>
      filter(!!sym("metric") %in% important_vars) |>
      mutate(
        metric = factor(
          !!sym("metric"),
          levels = important_vars
        )
      ) |>
      arrange(!!sym("metric"))
  } else if (table_type == "stored") {
    output_table <- custom_table|>
      filter(!(!!sym("metric") %in% important_vars))
  }
  return(output_table)
}

update_custom_tables <- function(input_table, model_permutation_importance, performance_metrics, table_options, r) {

  table_options <- match.arg(
    table_options,
    c("all", "important", "top_n")
  )

  if (table_options %in% c("important", "top_n")) {
    important_vars <- important_variables(
      model_permutation_importance = model_permutation_importance,
      performance_metrics = performance_metrics
    )

    all_important_variables <- create_scenario_table(
      custom_table <- input_table,
      important_vars = important_vars,
      table_type = "display"
    )

    all_remaining_variables <- create_scenario_table(
      custom_table <- input_table,
      important_vars = important_vars,
      table_type = "stored"
    )

    if (table_options == "important") {
      r$scenario_data$custom_display <- all_important_variables
      r$scenario_data$custom_stored <- all_remaining_variables
    } else if (table_options == "top_n") {
      n_value <- 15
      r$scenario_data$custom_display <- all_important_variables |>
        head(n_value)

      r$scenario_data$custom_stored <- all_important_variables |>
        tail(-n_value) |>
        bind_rows(all_remaining_variables)

    }
  } else if (table_options ==  "all") {
    r$scenario_data$custom_display <- input_table
    r$scenario_data$custom_stored <- input_table |>
      head(0)

  }

  r$scenario_data$custom <- bind_rows(
    r$scenario_data$custom_display,
    r$scenario_data$custom_stored
  )
}
