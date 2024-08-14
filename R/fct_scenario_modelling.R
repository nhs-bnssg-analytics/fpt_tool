#' scenario_modelling
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param scenario_data tibble; input data for the scenario being modelled -
#'   data comes from the table editting interface
#' @param ics_code character(1); three letter ICS code (beginning with Q)
#' @param model named list of model workflow objects whose names are the
#'   performance metrics
#' @importFrom tidyr pivot_wider pivot_longer complete
#' @importFrom dplyr select bind_cols mutate left_join setdiff bind_rows filter
#' @importFrom purrr lmap
#' @importFrom rlang sym
#' @import recipes
#' @importFrom glmnet glmnet
#' @importFrom randomForest randomForest
#' @noRd
model_scenario_data <- function(scenario_data, ics_code, model) {

  # performance data in wide format
  performance_data <- ics_data(
    ics_code = ics_code,
    domain_type = "Performance"
  ) |>
    select(
      "org",
      "year",
      "metric",
      "value"
    ) |>
    tidyr::pivot_wider(
      names_from = !!sym("metric"),
      values_from = !!sym("value")
    ) |>
    select(
      all_of(
        c("year", "org", names(model))
      )
    )

  # transform scenario data from wide format (where columns are year) to wider
  # format (where columns are metric).
  scenario_data <- scenario_data |>
    mutate(
      org = ics_code
    ) |>
    select(!c("theme")) |>
    tidyr::pivot_longer(
      cols = !c("metric", "org"),
      names_to = "year",
      names_transform = as.integer,
      values_to = "value"
    ) |>
    tidyr::pivot_wider(
      names_from = !!sym("metric"),
      values_from = !!sym("value")
    ) |>
    # join with performance data
    left_join(
      performance_data,
      by = join_by(
        !!sym("org"),
        !!sym("year")
      )
    ) |>
    # add 2 years of lag variables
    create_lag_variables() |>
    filter(
      !!sym("year") >= 2023
    ) |>
    # add dummy columns as they are needed in the model workflows
    mutate(
      nhs_region = NA_character_,
      quarter = NA_integer_,
      month = NA_integer_,
      .after = !!sym("year")
    )

  # identify which years in the scenario data (ie, the data which has been
  # supplied from the shiny app) are not in the observed performance data
  prediction_years <- setdiff(
    scenario_data$year,
    performance_data$year
  )

  # predict one year ahead at a time. This is particularly useful for the model
  # that predicts the change in value from one year to the next. It needs to
  # know the previous year's prediction to be able to make the subsequent year's
  # prediction
  for (yr in prediction_years) {

    next_scenario_year <- scenario_data |>
      filter(
        !!sym("year") <= yr
      )

    year_index <- next_scenario_year |>
      select("year", "org")
# browser()
    # create the predictions for the performance variables for the year of interest
    predictions <- purrr::lmap(
      model,
      .f = (\(x) make_predictions(x, input_data = next_scenario_year))
    ) |>
      setNames(
        nm = names(model)
      ) |>
      lapply(
        bind_cols,
        year_index
      ) |>
      bind_rows(
        .id = "metric"
      ) |>
      mutate(
        .pred = !!sym(".pred") * 100
      ) |>
      select(
        "metric",
        "year",
        "org",
        value = ".pred"
      ) |>
      filter(
        !!sym("year") == yr
      ) |>
      # extend the table to future years so the lag versions of the performance
      # variables can be filled
      tidyr::complete(
        year = seq(
          from = yr,
          to = yr + 2,
          by = 1
        ),
        !!sym("org"),
        !!sym("metric")
      ) |>
      tidyr::pivot_wider(
        names_from = !!sym("metric"),
        values_from = !!sym("value")
      ) |>
      create_lag_variables()

    scenario_data <- update_scenario_performance_data_with_predictions(
      scenario_data = scenario_data,
      prediction_data = predictions
    )
  }

  predictions <- scenario_data |>
    tidyr::pivot_longer(
      cols = !c("year", "org", "nhs_region", "quarter", "month"),
      names_to = "metric",
      values_to = "value"
    ) |>
    filter(
      !!sym("metric") %in% names(performance_data)
    )

  return(predictions)

}


#' add fields for lagged versions of the variables
#'
#' @param data tibble to create lagged variables from
#' @param lagged_years number of years to lag
#' @importFrom rlang set_names sym
#' @importFrom purrr partial map map_df
#' @importFrom dplyr arrange across any_of group_by group_split mutate
#'   lag
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
    group_by(!!sym("org")) |>
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


#' Replace values in the scenario dataset with predicted values in the
#' prediction dataset. This includes lagged versions of the metrics. Any values
#' in the scenario dataset should be retained, and only NAs should be replaced
#'
#' @param scenario_data a wide tibble with fields "org", "year", "nhs_region",
#'   "quarter", "month" and then a field for any metric
#' @param prediction_data a wider dataset with field for "org" and "year" and
#'   then a field for any metric
#'
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom dplyr filter setdiff rows_update
#' @importFrom rlang sym
#'
#' @return a tibble of identical dimensions to the scenario dataset, but with
#'   some values for the metrics replaced
#' @noRd
update_scenario_performance_data_with_predictions <- function(scenario_data, prediction_data) {
  required_scenario_names <- c("org", "year", "nhs_region", "quarter", "month")
  if (!all(required_scenario_names %in% names(scenario_data))) {
    missing_names <- setdiff(
      required_scenario_names,
      names(scenario_data)
    )
    stop(
      paste(
        paste(missing_names, collapse = ", "),
        "fields are missing from scenario_data"
      )
    )
  }

  required_prediction_names <- c("org", "year")
  if (!all(required_prediction_names %in% names(prediction_data))) {
    missing_names <- setdiff(
      required_prediction_names,
      names(prediction_data)
    )
    stop(
      paste(
        paste(missing_names, collapse = ", "),
        "fields are missing from prediction_data"
      )
    )
  }

  prediction_data_long <- prediction_data |>
    pivot_longer(
      cols = !c("org", "year"),
      names_to = "metric",
      values_to = "value"
    ) |>
    filter(!is.na(!!sym("value")))

  scenario_data <- scenario_data |>
    pivot_longer(
      cols = !c("org", "year", "nhs_region", "quarter", "month"),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::rows_update(
      y = prediction_data_long,
      by = c("year", "org", "metric"),
      unmatched = "ignore" # this occurs for the final years of scenario_data as the lagged prediction fields extend beyond the time period in scenario data
    ) |>
    tidyr::pivot_wider(
      names_from = !!sym("metric"),
      values_from = !!sym("value")
    )

  return(scenario_data)
}


#' Understand the model object
#'
#' @param model named list containing workflow object as only item. The name
#'   should be the target variable name
#'
#' @return tibble with two fields, engine, which describes the type of model
#'   (eg, glmnet or randomForest), and model_type, which describe whether the
#'   model is modelling a proportion or a difference from a previous value
#'
#' @details to understand whether the model type is a difference from previous
#'   value model, the function looks for whether any of the input variables have
#'   a negative value. If they do, then the model is considered a "difference
#'   from previous" model
#'
#' @importFrom tune extract_mold extract_fit_engine
#' @importFrom purrr pluck
#' @importFrom dplyr tibble where select
#' @noRd
model_descriptions <- function(model) {
  engine <- model |>
    tune::extract_fit_engine() |>
    class()

  negative_values <- model |>
    # tune::extract_mold() |>
    purrr::pluck("pre", "actions", "recipe", "recipe", "template") |>
    select(!any_of(c("total_cases", "month", "quarter", "year", "nhs_region"))) |>
    purrr::map_lgl(
      # test if any column has a negative value in it
      \(x) any(x < 0, na.rm = TRUE)
    ) |>
    any()

  model_type <- ifelse(negative_values, "difference", "proportion")

  output <- tibble(
    engine = engine[1],
    model_type = model_type
  )
  return(output)
}


#' Caculate predictions from model and input data
#'
#' @param model named list containing workflow object as only item. The name
#'   should be the target variable name
#' @param input_data tibble where all the field are input variables for the
#'   models
#'
#' @details the function understands from the components of the workflow object
#'   whether the model is modelling a proportion target variable or a difference
#'   from previous year. If it is the latter, it will apply the difference to
#'   the previous year's value to create the prediction.
#'
#' @return tibble with one field, named .pred, which is the prediction for the
#'   target variable defined by the name of the list item passed in through the
#'   model object
#'
#' @importFrom purrr pluck
#' @importFrom dplyr select all_of bind_cols mutate lag row_number pick
#'   everything contains slice
#' @importFrom rlang sym
#' @importFrom tune extract_fit_parsnip
#' @importFrom stats predict
#' @noRd
make_predictions <- function(model, input_data) {

  target_variable <- names(model)

  # get workflow from model list
  wf <- model |>
    purrr::pluck(1)

  model_configuration <- model_descriptions(wf)

  input_data <- input_data |>
    mutate(
      across(
        contains(target_variable),
        \(x) x / 100
      )
    )

  if ("difference" %in% model_configuration$model_type) {

    observed_target <- input_data |>
      select(
        all_of(target_variable)
      )

    input_data <- input_data |>
      arrange(!!sym("org"), !!sym("year")) |>
      mutate(
        across(
          !any_of(c("year", "quarter", "month", "org", "nhs_region", "pandemic_onwards")),
          \(x) x - lag(x)
        ),
        .by = c(
          !!sym("org")
        )
      )
  }

  if (grepl("glm", model_configuration$engine)) {
    lambda <- wf |>
      tune::extract_fit_parsnip() |>
      pluck("spec", "args", "penalty")

    prediction <- stats::predict(
      wf,
      new_data = input_data,
      penalty = lambda
    )
  } else if (grepl("random", model_configuration$engine)) {
    # for RF, it doesn't default to providing NA when it can't make a prediction
    # because the input data aren't correct (eg, missing data which haven't been
    # imputed in the workflow). This function helps it return an NA in this
    # situation
    predict_with_error_handle <- function(object, inputs) {

      data_cols <- setdiff(
        names(inputs),
        c("org", "year", "nhs_region", "quarter", "month")
      )

      all_na <- inputs |>
        select(
          all_of(
            data_cols
          )
        ) |>
        is.na() |>
        all()

      if (isTRUE(all_na)) {
        pred <- NA
      } else {
        pred <- try(
          stats::predict(object, new_data = inputs),
          silent = TRUE
        )

        if (inherits(pred, "try-error")) {
          pred <- NA
        } else {
          pred <- as.numeric(pred$.pred)
        }
      }

      return(pred)
    }

    prediction <- input_data %>%
      mutate(
        id = dplyr::row_number()
      ) |>
      mutate(
        .pred = predict_with_error_handle(
          object = wf,
          inputs = dplyr::pick(
            dplyr::everything()
          )
        ),
        .by = !!sym("id")
      ) |>
      dplyr::select(".pred")
  }


  if ("difference" %in% model_configuration$model_type) {

    prediction <- bind_cols(
      prediction, observed_target
    ) |>
      mutate(
        # add the predicted change to the lag value for the target variable
        .pred = !!sym(".pred") + lag(!!sym(target_variable))
      ) |>
      select(!all_of(c(target_variable)))
  }

  return(prediction)
}
