#' scenario_modelling
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param scenario_data tibble; input data for the scenario being modelled -
#'   data comes from the table editting interface
#' @param ics_code character(1); three letter ICS code (beginning with Q)
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr select bind_cols mutate left_join
#' @importFrom parsnip predict.model_fit
#' @importFrom rlang sym
#' @import recipes
#' @import glmnet
#' @noRd
model_scenario_data <- function(scenario_data, ics_code) {
  # this will have to read multiple model objects - or I store all the objects in one rds file?
  # model <- readRDS("C:/Users/Sebastian.Fox/Documents/R/Play/d_and_c/outputs/model_objects/Proportion of A&E attendances greater than 4 hours (Type 1 Departments - Major A&E).rds")

  model <- readRDS("C:/Users/Sebastian.Fox/Documents/R/Play/d_and_c/outputs/model_objects/wfs.rds")

  performance_data <- ics_data(
    ics_code = ics_code,
    domain_type = "Performance"
  ) |>
    select(
      "year",
      "metric",
      "value"
    ) |>
    tidyr::pivot_wider(
      names_from = !!sym("metric"),
      values_from = !!sym("value")
    )

  scenario_data <- scenario_data |>
    select(!c("domain")) |>
    tidyr::pivot_longer(
      cols = !c("metric"),
      names_to = "year",
      names_transform = as.integer,
      values_to = "value"
    ) |>
    tidyr::pivot_wider(
      names_from = !!sym("metric"),
      values_from = !!sym("value")
    ) |>
    mutate(
      org = ics_code,
      nhs_region = NA_character_,
      quarter = NA_integer_,
      month = NA_integer_
    ) |>
    left_join(
      performance_data,
      by = join_by(!!sym("year"))
    ) |>
    create_lag_variables()

  predictions <- lapply(
    model,
    predict,
    new_data = scenario_data
    ) |>
    lapply(
      bind_cols,
      scenario_data
    ) |>
    bind_rows(
      .id = "metric"
    ) |>
    select(
      "metric",
      "year",
      value = ".pred"
    ) |>
    mutate(
      org = ics_code,
      # metric = target_metric,
      value_type = "prediction"
    )

  return(predictions)

}


#' add fields for lagged versions of the variables
#'
#' @param data tibble to create lagged variables from
#' @param lagged_years number of years to lag
#' @importFrom rlang set_names
#' @importFrom purrr partial map map_df
#' @importFrom dplyr arrange across any_of group_by group_split mutate
#'   everything lag
#' @return tibble with additional columns to the input tibble that represent
#'   lagged versions of the input data
#' @noRd
create_lag_variables <- function(data, lagged_years = 2) {
  map_lag <- set_names(
    seq_len(lagged_years),
    nm = paste("lag", seq_len(lagged_years), sep = "_")
  ) |>
    map(
      ~ purrr::partial(lag, n = .x)
    )

  lag_vars <- setdiff(
    names(data),
    c("org", "year")
  )

  data <- data |>
    arrange(
      across(
        any_of(
          c("year", "quarter", "month", "org")
        )
      )
    ) |>
    group_by(org) |>
    group_split() |>
    purrr::map_df(
      ~ mutate(
        .x,
        across(
          all_of(lag_vars),
          .fn = map_lag,
          .names = "{.fn}_{.col}"
        )
      )
    )

  return(data)
}
