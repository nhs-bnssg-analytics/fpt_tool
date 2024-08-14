test_that("ics_names internal data", {
  expect_equal(
    names(ics_names),
    c("East of England", "London", "Midlands", "North East and Yorkshire",
      "North West", "South East", "South West"),
    label = "all 7 regions are in ics names"
  )
})

test_that("trust_ics_props internal data", {
  expect_equal(
    trust_ics_props |>
      dplyr::count(ICB22CDH) |>
      nrow(),
    42,
    label = "42 ICBs in trust to icb lookup"
  )

  expect_equal(
    names(trust_ics_props),
    c("TrustCode", "TrustName", "ICB22CDH", "proportion"),
    label = "expected names in trust_ics_props"
  )
})


test_that("metadata internal data", {

  expect_equal(
    names(metadata),
    c("metric", "domain", "theme", "numerator_description", "denominator_description"),
    label = "expected names in metadata"
  )
})


test_that("ics_timeseries internal data", {

  expect_equal(
    names(ics_timeseries),
    c("year", "org", "domain", "theme", "metric", "numerator", "denominator", "value"),
    label = "expected names in ics_timeseries"
  )
})

test_that("model_accuracy internal data", {

  expect_equal(
    names(model_accuracy),
    c("target_variable", "mape"),
    label = "expected names in model_accuracy"
  )
})

test_that("model internal data", {

  expect_equal(
    length(names(model)),
    5,
    label = "expected length of model object"
  )
})

test_that("lookup_ics_table internal data", {

  expect_equal(
    names(lookup_ics_table),
    c("ICB22CD", "ICB22CDH", "ICB22NM", "NHSER22NM"),
    label = "expected names in lookup_ics_table object"
  )
})
