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
