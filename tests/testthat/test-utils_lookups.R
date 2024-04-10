test_that("42 ICSs returned", {
  expect_equal(42, length(lookup_ics_names()))
})


test_that("ICS code to name lookup is correct", {
  expect_equal(
    lookup_ics_names("NHS Bristol, North Somerset and South Gloucestershire Integrated Care Board"),
    "QUY"
  )
})
