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
    c("metric", "domain"),
    info = "metric and domain are first 2 column names for scenario_inputs() 'last_known_year'"
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
    c("metric", "domain"),
    info = "metric and domain are first 2 column names for scenario_inputs() 'percent_change'"
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
    c("metric", "domain"),
    info = "metric and domain are first 2 column names for scenario_inputs() 'linear'"
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
    c("metric", "domain"),
    info = "metric and domain are the character fields in the reset_scenarios function"
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

test_that("create_scenario_table works as expected", {
  tbl <- tibble(
    metric = letters[1:13]
  )

  imp_vars <- c("a", "c", "b")

  expect_equal(
    create_scenario_table(
      custom_table = tbl,
      important_vars = imp_vars,
      table_type = "display"
    ) |>
      pull(metric) |>
      as.character(),
    imp_vars,
    info = "the table produced by create_scenario_table is ordered in the same way as the input important variables when table_type is 'display'"
  )

  expect_length(
    create_scenario_table(
      custom_table = tbl,
      important_vars = imp_vars,
      table_type = "display"
    ) |>
      pull(metric) |>
      as.character() |>
      setdiff(imp_vars),
    0
  )

  expect_length(
    create_scenario_table(
      custom_table = tbl,
      important_vars = imp_vars,
      table_type = "stored"
    ) |>
      pull(metric) |>
      as.character() |>
      setdiff(imp_vars),
    # the length of the stored table is the same as the original with the
    # important variable removed
    nrow(tbl) - length(imp_vars)
  )

})
