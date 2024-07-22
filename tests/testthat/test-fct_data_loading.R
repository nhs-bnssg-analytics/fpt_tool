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
