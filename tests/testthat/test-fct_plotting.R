test_that("ggplot images are consistent", {
  metrics <- performance_metrics()

  input <- ics_data(
    ics_code = "QUY",
    domain_type = "Performance"
  )

  p <- plot_performance(
    historic_data = input,
    performance_metric = metrics
  )

  vdiffr::expect_doppelganger(
    "4 performance metrics",
    p
  )
})

test_that("pie chart works", {

  vdiffr::expect_doppelganger(
    "Acute to trust proportions pie chart is consistent",
    plot_trust_icb_proportions("QR1")
  )
})

test_that("hold message image works", {
  vdiffr::expect_doppelganger(
    "ggplot hold message",
    plot_hold_message()
  )
})
