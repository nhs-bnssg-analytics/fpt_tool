test_that("dimensions of ics_data function", {
  bnssg_data <- ics_data(
    ics_code = "QUY"
  )
  expect_equal(
    names(bnssg_data),
    c("year", "org", "domain", "theme", "metric", "numerator",
      "denominator", "value", "value_type"),
    label = "all field names are as expected"
  )

  expect_gt(
    nrow(bnssg_data),
    1,
    label = "more than one row of data for BNSSG"
  )
})

test_that("narrow age bands are replaced by broad age bands using replace_narrow_age_bands", {

  narrow_age_bands <- c("0-9",
                        "10-19",
                        "20-29",
                        "30-39",
                        "40-49",
                        "50-59",
                        "60-69",
                        "70-79",
                        "80-89",
                        "90+")

  broad_age_bands <- c("0-29", "30-59", "60+")

  demand_timeseries <- tibble(
    metric = paste(
      "Proportion of population in age band ",
      narrow_age_bands
    ),
    numerator = sample(1:10, 10, replace = TRUE),
    denominator = sample(50:70, 10, replace = TRUE)
  )
  narrow_demand_timeseries <- demand_timeseries |>
    replace_narrow_age_bands()

  expect_true(
    all(
      purrr::map_lgl(
        broad_age_bands,
        ~ any(grepl(.x, unique(narrow_demand_timeseries$metric)))
      )
    ),
    info = "each of the age bands are in the broad age band version of population data"
  )

  expect_true(
    !all(
      purrr::map_lgl(
        narrow_age_bands,
        ~ any(grepl(.x, unique(narrow_demand_timeseries$metric)))
      )
    ),
    info = "none of the age bands are in the narrow age band version of population data"
  )


})



test_that("testing errors", {
  expect_error(
    ics_data("QUY", domain_type = "performance"),
    "domain_type needs to be one of 'Performance', 'Demand' or 'Capacity'"
  )
})

