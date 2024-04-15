#' scenario_modelling
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param scenario_data tibble; input data for the scenario being modelled -
#'   data comes from the table editting interface
#' @param target_metric character(1); name of metric being modelled which will be used in the final table
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr select bind_cols mutate
#' @importFrom parsnip predict.model_fit
#' @import recipes
#' @import glmnet
#' @noRd
model_scenario_data <- function(scenario_data, target_metric, ics_code) {
  # this will have to read multiple model objects - or I store all the objects in one rds file?
  model <- readRDS("C:/Users/Sebastian.Fox/Documents/R/Play/d_and_c/outputs/model_objects/Proportion of A&E attendances greater than 4 hours (Type 1 Departments - Major A&E).rds")

  scenario_data <- scenario_data |>
    select(!c("domain")) |>
    tidyr::pivot_longer(
      cols = !c("metric"),
      names_to = "year",
      names_transform = as.integer,
      values_to = "value"
    ) |>
    tidyr::pivot_wider(
      names_from = metric,
      values_from = value
    ) |>
    mutate(
      org = NA_character_,
      nhs_region = NA_character_,
      quarter = NA_integer_,
      month = NA_integer_
    )

  predictions <- model |>
    predict(
      new_data = scenario_data
    ) |>
    bind_cols(
      scenario_data
    ) |>
    select(
      "year",
      value = ".pred"
    ) |>
    mutate(
      org = ics_code,
      metric = target_metric,
      value_type = "prediction"
    )

  return(predictions)

}
