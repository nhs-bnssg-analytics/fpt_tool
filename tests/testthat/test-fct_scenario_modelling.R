test_that("lagging function works", {
  input <- tibble::tribble(
             ~org, ~year, ~metric1, ~metric2,
              "a", 2015L,       2L,       3L,
              "a", 2016L,       4L,       8L,
              "a", 2017L,       4L,      10L,
              "a", 2018L,       0L,       0L,
              "a", 2019L,       6L,       7L,
              "b", 2015L,       8L,       2L,
              "b", 2016L,       3L,       1L,
              "b", 2017L,       8L,       6L,
              "b", 2018L,       7L,       3L,
              "b", 2019L,       5L,       5L
             )


  output <- tibble::tribble(
     ~org, ~year, ~metric1, ~metric2, ~lag_1_metric1, ~lag_2_metric1, ~lag_1_metric2, ~lag_2_metric2,
      "a", 2015L,       2L,       3L,             NA,             NA,             NA,             NA,
      "a", 2016L,       4L,       8L,             2L,             NA,             3L,             NA,
      "a", 2017L,       4L,      10L,             4L,             2L,             8L,             3L,
      "a", 2018L,       0L,       0L,             4L,             4L,            10L,             8L,
      "a", 2019L,       6L,       7L,             0L,             4L,             0L,            10L,
      "b", 2015L,       8L,       2L,             NA,             NA,             NA,             NA,
      "b", 2016L,       3L,       1L,             8L,             NA,             2L,             NA,
      "b", 2017L,       8L,       6L,             3L,             8L,             1L,             2L,
      "b", 2018L,       7L,       3L,             8L,             3L,             6L,             1L,
      "b", 2019L,       5L,       5L,             7L,             8L,             3L,             6L
     )



  output_test <- create_lag_variables(input)

  expect_equal(
    output_test,
    output
  )
})

test_that("update scenario_data function works", {
  input_scenario <- tibble::tribble(
    ~org, ~year, ~nhs_region, ~quarter, ~month, ~metric_a, ~metric_b,
    "QPG", 2017L,          NA,       NA,     NA,       91L,       46L,
    "QPG", 2018L,          NA,       NA,     NA,        8L,       55L,
    "QPG", 2019L,          NA,       NA,     NA,       24L,       91L,
    "QPG", 2020L,          NA,       NA,     NA,       40L,       99L,
    "QPG", 2021L,          NA,       NA,     NA,        NA,        NA,
    "QPG", 2022L,          NA,       NA,     NA,        NA,        NA,
    "QPG", 2023L,          NA,       NA,     NA,        NA,        NA
  )

  input_predictions <- tibble::tribble(
    ~org, ~year, ~metric_a, ~metric_b,
    "QPG", 2022L,       12L,        4L,
    "QPG", 2023L,       66L,       85L
  )

  expected_result <- tibble::tribble(
    ~org, ~year, ~nhs_region, ~quarter, ~month, ~metric_a, ~metric_b,
    "QPG", 2017L,          NA,       NA,     NA,       91L,       46L,
    "QPG", 2018L,          NA,       NA,     NA,        8L,       55L,
    "QPG", 2019L,          NA,       NA,     NA,       24L,       91L,
    "QPG", 2020L,          NA,       NA,     NA,       40L,       99L,
    "QPG", 2021L,          NA,       NA,     NA,        NA,        NA,
    "QPG", 2022L,          NA,       NA,     NA,       12L,        4L,
    "QPG", 2023L,          NA,       NA,     NA,       66L,       85L
  )

  output_test <- update_scenario_performance_data_with_predictions(
    scenario_data = input_scenario,
    prediction_data = input_predictions
  )

  expect_equal(
    output_test,
    expected_result,
    label = "update_scenario_performance_data_with_predictions works as expected"
  )

  expect_error(
    update_scenario_performance_data_with_predictions(
      scenario_data = input_scenario |>
        select(!c("nhs_region")),
      prediction_data = input_predictions
    ),
    "nhs_region fields are missing from scenario_data",
    label = "check that all fields are present in scenario_data"
  )

  expect_error(
    update_scenario_performance_data_with_predictions(
      scenario_data = input_scenario,
      prediction_data = input_predictions |>
        select(!c("org"))
    ),
    "org fields are missing from prediction_data",
    label = "check that all fields are present in prediction_data"
  )
})

test_that("end to end modelling function works", {
  ics <- "QUY"
  inputs <- scenario_inputs(
    ics_code = ics,
    horizon = 4,
    scenario = "last_known_year"
  )

  model_outputs <- readRDS("C:/Users/Sebastian.Fox/Documents/R/Play/d_and_c/outputs/model_objects/wfs_log_pi.rds") |>
    lapply(
      function(x) x[[1]]
    )

  outputs <- model_scenario_data(
    scenario_data = inputs,
    ics_code = ics,
    model = model_outputs
  )

  expect_equal(
    names(outputs),
    c("org", "year", "nhs_region", "quarter", "month", "metric", "value"),
    label = "checking output names of the model_scenario_data function"
  )

  expect_equal(
    sum(
      colSums(
        is.na(
          outputs |>
            select("org", "year", "metric", "value")
        )
      )
    ),
    0,
    label = "checking no NAs in important fields"
  )

  expect_length(
    setdiff(
      outputs$year,
      inputs |>
        pivot_longer(
          cols = !c("metric", "domain"),
          names_to = "year",
          values_to = "value"
        ) |>
        pull(year)
    ),
    0
  )
})
