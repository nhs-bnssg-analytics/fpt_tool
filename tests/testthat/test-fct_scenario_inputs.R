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
