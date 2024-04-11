test_that("42 ICSs returned from lookup_ics_names()", {
  expect_equal(42, length(lookup_ics_names()))
})


test_that("ICS code to name lookup is correct", {
  expect_equal(
    ics_code_lkp("NHS Bristol, North Somerset and South Gloucestershire Integrated Care Board"),
    "QUY"
  )
})

test_that("ICS code to name lookup is correct", {
  expect_equal(
    ics_name_lkp("QUY"),
    "NHS Bristol, North Somerset and South Gloucestershire Integrated Care Board"
  )
})


test_that("ICS names list or vector is correct data type", {
  expect_type(
    lookup_ics_names(with_regions = FALSE),
    "character"
  )
  expect_type(
    lookup_ics_names(with_regions = TRUE),
    "list"
  )
})
