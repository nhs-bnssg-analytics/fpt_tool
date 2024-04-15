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
