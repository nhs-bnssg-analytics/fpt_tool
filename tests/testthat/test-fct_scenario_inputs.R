model_outputs <- load_model_object()

test_that("error handling", {
  expect_error(
    scenario_inputs(
      ics_code = "QUY",
      horizon = 1,
      scenario = "linear"),
    "linear_years must not be missing when linear is applied",
    info = "linear extrapolation isn't missing the number of years to extrapolate from"
  )
  expect_error(
    scenario_inputs(
      ics_code = "QUY",
      horizon = 1,
      scenario = "percent"),
    "percent must not be missing when percent_change is applied",
    info = "percent extrpolation has the percentage change as an input"
  )
})


test_that("data dimensions last_known_year ", {
  df <- scenario_inputs(
    ics_code = "QUY",
    horizon = 1,
    scenario = "last_known_year")

  expect_gt(
    ncol(df),
    2
  )
  expect_equal(
    names(df)[1:2],
    c("metric", "theme"),
    info = "metric and theme are first 2 column names for scenario_inputs() 'last_known_year'"
  )
  expect_equal(
    df |> dplyr::count(metric) |> filter(n > 1) |> nrow(),
    0,
    info = "no duplicate metrics returns from scenario_inputs() 'last_known_year'"
  )

})

test_that("data dimensions percent_change ", {
  df <- scenario_inputs(
    ics_code = "QUY",
    horizon = 1,
    scenario = "percent_change",
    percent = 0.5)

  expect_gt(
    ncol(df),
    2
  )
  expect_equal(
    names(df)[1:2],
    c("metric", "theme"),
    info = "metric and theme are first 2 column names for scenario_inputs() 'percent_change'"
  )
  expect_equal(
    df |> dplyr::count(metric) |> filter(n > 1) |> nrow(),
    0,
    info = "no duplicate metrics returns from scenario_inputs() 'percent_change'"
  )

})

test_that("data dimensions linear ", {
  df <- scenario_inputs(
    ics_code = "QUY",
    horizon = 1,
    scenario = "linear",
    linear_years = 3)

  expect_gt(
    ncol(df),
    2
  )
  expect_equal(
    names(df)[1:2],
    c("metric", "theme"),
    info = "metric and theme are first 2 column names for scenario_inputs() 'linear'"
  )
  expect_equal(
    df |> dplyr::count(metric) |> filter(n > 1) |> nrow(),
    0,
    info = "no duplicate metrics returns from scenario_inputs() 'linear'"
  )

})


test_that("reset_scenarios works as expected", {

  scenarios <- reset_scenarios(
    ics_cd = "QHL",
    horizon = 1,
    linear_years = 3,
    percent = 0.5
  )

  expect_equal(
    names(scenarios),
    c("last_known", "percent", "linear", "custom"),
    info = "the names of the output from the reset_scenarios function are what is expected"
  )

  expect_true(
    all(
      sapply(
        scenarios[-1], function(x) identical(names(scenarios[[1]]), names(x))
      )
    ),
    info = "all tables in the list items have identical names"
  )

  expect_true(
    all(
      sapply(
        scenarios[-1], function(x) identical(dim(scenarios[[1]]), dim(x))
      )
    ),
    info = "all tables in the list items have identical dimension"
  )

  expect_equal(
    scenarios |>
      map(
        \(x) names(
          dplyr::select(
            x,
            dplyr::where(
              is.character
            )
          )
        )
      ) |>
      unlist() |>
      unique(),
    c("metric", "theme"),
    info = "metric and theme are the character fields in the reset_scenarios function"
  )

})


test_that("important_variables() works as expected", {

  perm_imp <- model_outputs |>
    lapply(
      \(x) x[["perm_imp"]]
    )

  expect_error(
    important_variables(
      model_permutation_importance = perm_imp,
      performance_metrics = "unmodelled metric"
    ),
    "not all the inputs for performance_metrics have associated models",
    info = "incorrect metric applied to important_variables causes an error"
  )

  expect_identical(
    important_variables(
      model_permutation_importance = perm_imp,
      performance_metrics = performance_metrics()[1:2],
      top_n = 3
    ) |>
      class(),
    "character",
    info = "class of important_variables() output is character"
  )

  expect_identical(
    important_variables(
      model_permutation_importance = perm_imp,
      performance_metrics = performance_metrics()[1:2],
      top_n = 3
    ) |>
      length(),
    3L,
    info = "length of important_variables() output is the same as what is specified by top_n"
  )

})

test_that("the scenario data checker function ensures all values that are not real have been changed", {
  dummy_metrics <- c(
    "proportion of a",
    "prevalence of purple",
    "fte per 1000",
    "sixty seven",
    "% 70"
  )

  inputs <- tibble(
    metric = dummy_metrics,
    theme = c("Capacity", "Performance", "Demand", "Capacity", "Demand")
  ) |>
    cross_join(
      tibble(
        year = 2021:2023
      )
    ) |>
    mutate(
      value = purrr::map_dbl(
        metric,
        ~ case_when(
          .x == dummy_metrics[1] ~ sample(110:200, 1),
          .x == dummy_metrics[2] ~ sample(5:20, 1),
          .x == dummy_metrics[3] ~ sample(-15:-1, 1),
          .x == dummy_metrics[4] ~ sample(300:600, 1),
          .x == dummy_metrics[5] ~ sample(0:10, 1),
          .default = NA_real_
        )
      )
    ) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = value
    )

  historic_data <- inputs |>
    distinct(
      metric, theme
    ) |>
    cross_join(
      tibble(
        year = 2015:2020
      )
    ) |>
    mutate(
      value = purrr::map_dbl(
        metric,
        ~ case_when(
          .x == dummy_metrics[1] ~ sample(110:200, 1),
          .x == dummy_metrics[2] ~ sample(5:20, 1),
          .x == dummy_metrics[3] ~ sample(-15:-1, 1),
          .x == dummy_metrics[4] ~ sample(300:600, 1),
          .x == dummy_metrics[5] ~ sample(0:10, 1),
          .default = NA_real_
        )
      )
    )

  test <- check_scenario_inputs(
    inputs,
    historic_data = historic_data
  )

  expect_equal(
    dim(inputs),
    dim(test),
    info = "check_scenario_inputs doesn't change the shape of the table passed to it"
  )

  expect_equal(
    test |>
      dplyr::select(dplyr::where(is.numeric)) |>
      (\(x) x < 0)() |>
      colSums() |>
      sum(),
    0,
    info = "there are no values less than zero after being passed through check_scenario_inputs"
  )

  expect_equal(
    test |>
      filter(
        grepl("prevalence|proportion|%", !!sym("metric"))
      ) |>
      dplyr::select(dplyr::where(is.numeric)) |>
      (\(x) x > 100)() |>
      colSums() |>
      sum(),
    0,
    info = "there are no values greater than 100 for proportion metrics after being passed through check_scenario_inputs"
  )

})



# predictions testing -----------------------------------------------------

test_that("update_prediction_and_plot_r", {

  ics_code <- "QUY"
  all_ics_data <- ics_data(
    ics_code
  )


  scenario_data_inputs <- scenario_inputs(
    ics_code = ics_code,
    horizon = 4,
    scenario = "last_known"
  )

  scenario <- "custom"
  scenario_data <- list()
  scenario_data[[scenario]] <- scenario_data_inputs

  testing <- update_predictions_and_plot_r(
    prediction_custom_scenario = "testing",
    model_outputs = load_model_object("wf"),
    scenario_name = scenario,
    performance_metrics = "Proportion of attended GP appointments (over 4 weeks wait time)",
    r = list(
      ics_cd = ics_code,
      ics_data = all_ics_data,
      scenario_data = scenario_data
    )
  )

  predictions <- purrr::pluck(
    testing, "predictions"
  )

  # test incorrect scenario_name
  expect_snapshot(
    update_predictions_and_plot_r(
      prediction_custom_scenario = "testing",
      model_outputs = load_model_object("wf"),
      scenario_name = "incorrect_name",
      performance_metrics = "Proportion of attended GP appointments (over 4 weeks wait time)",
      r = list(
        ics_cd = ics_code,
        ics_data = all_ics_data,
        scenario_data = scenario_data
      )
    ),
    error = TRUE
  )

  # test duplicate custom scenario name??
  expect_snapshot(
    custom_test <- update_predictions_and_plot_r(
      prediction_custom_scenario = "testing",
      model_outputs = load_model_object("wf"),
      scenario_name = scenario,
      performance_metrics = "Proportion of attended GP appointments (over 4 weeks wait time)",
      r = list(
        ics_cd = ics_code,
        ics_data = all_ics_data,
        scenario_data = scenario_data,
        predictions = predictions
      )
    ),
    error = TRUE
  )
})

test_that("multiple custom scenarios", {
  scenario <- "custom"
  ics_code <- "QUY"
  scenario_data_inputs <- scenario_inputs(
    ics_code = ics_code,
    horizon = 4,
    scenario = "last_known"
  )

  scenario_data <- list()
  scenario_data[[scenario]] <- scenario_data_inputs

  all_ics_data <- ics_data(
    ics_code
  )

  testing <- update_predictions_and_plot_r(
    prediction_custom_scenario = "testing",
    model_outputs = load_model_object("wf"),
    scenario_name = scenario,
    performance_metrics = "Proportion of attended GP appointments (over 4 weeks wait time)",
    r = list(
      ics_cd = ics_code,
      ics_data = all_ics_data,
      scenario_data = scenario_data
    )
  )

  predictions <- purrr::pluck(
    testing, "predictions"
  )
  # test multiple custom scenarios
  expect_equal(
    update_predictions_and_plot_r(
      prediction_custom_scenario = "testing",
      model_outputs = load_model_object("wf"),
      scenario_name = scenario,
      performance_metrics = "Proportion of attended GP appointments (over 4 weeks wait time)",
      r = list(
        ics_cd = ics_code,
        ics_data = all_ics_data,
        scenario_data = scenario_data,
        predictions = predictions |>
          mutate(
            value_type = "Prediction - old"
          )
      )
    ) |>
      purrr::pluck("predictions") |>
      dplyr::distinct(!!sym("value_type")) |>
      pull(),
    c("Prediction - old", "Prediction - testing"),
    label = "multiple custom scenarios"
  )

  # test that there are no duplicate years for predictions within scenario type for custom scenario
  expect_equal(
    predictions |>
      dplyr::count(!!sym("year"), !!sym("value_type")) |>
      dplyr::pull() |>
      unique(),
    1,
    label = "test that there are no duplicate years for predictions within scenario type"
  )

  expect_equal(
    names(testing),
    c("ics_cd", "ics_data", "scenario_data", "predictions", "performance_plot"),
    label = "test that the outputs object contains all of the correct named items"
  )
})


test_that("overwriting previous template scenario", {
  # testing non-custom scenario

  ics_code <- "QUY"
  scenario <- "last_known"
  scenario_data_inputs <- scenario_inputs(
    ics_code = ics_code,
    horizon = 4,
    scenario = scenario
  )

  scenario_data <- list()
  scenario_data[[scenario]] <- scenario_data_inputs

  all_ics_data <- ics_data(
    ics_code
  )

  testing_last_known <- update_predictions_and_plot_r(
    prediction_custom_scenario = "testing",
    model_outputs = load_model_object("wf"),
    scenario_name = scenario,
    performance_metrics = "Proportion of attended GP appointments (over 4 weeks wait time)",
    r = list(
      ics_cd = ics_code,
      ics_data = all_ics_data,
      scenario_data = scenario_data
    )
  )

  # create dummy value to test whether it is replaced when same scenario is run
  dummy_replacement_value <- 1
  dummy_last_known_predictions <- purrr::pluck(
    testing_last_known,
    "predictions"
  ) |>
    mutate(
      value = dummy_replacement_value
    )

  expect_false(
    any(
      update_predictions_and_plot_r(
        prediction_custom_scenario = "testing",
        model_outputs = load_model_object("wf"),
        scenario_name = scenario,
        performance_metrics = "Proportion of attended GP appointments (over 4 weeks wait time)",
        r = list(
          ics_cd = ics_code,
          ics_data = all_ics_data,
          scenario_data = scenario_data,
          predictions = dummy_last_known_predictions
        )
      ) |>
        purrr::pluck("predictions") |>
        pull(!!sym("value")) == dummy_replacement_value
    ),
    label = "the dummy replacement value is replaced by the regenerated prediction values"
  )
})





test_that("update_custom_tables works", {

  ics_code <- "QUY"
  scenario <- "last_known"
  scenario_data_inputs <- scenario_inputs(
    ics_code = ics_code,
    horizon = 4,
    scenario = scenario
  )

  model_pi <- load_model_object("perm_imp")

  performance_metrics <- "Proportion of attended GP appointments (over 4 weeks wait time)"

  r <- list()

  output_r_table <- update_custom_tables(
    input_table = scenario_data_inputs,
    model_permutation_importance = model_pi,
    performance_metrics = performance_metrics,
    r = r
  )

  expect_contains(
    names(output_r_table),
    "scenario_data"
  )

  expect_contains(
    output_r_table |> purrr::pluck("scenario_data") |> names(),
    "custom"
  )

  expect_equal(
    output_r_table |>
      purrr::pluck("scenario_data", "custom") |>
      names() |>
      head(2),
    c("metric", "theme"),
    label = "metric and theme are the first two headers of scenario_data$custom"
  )

})
