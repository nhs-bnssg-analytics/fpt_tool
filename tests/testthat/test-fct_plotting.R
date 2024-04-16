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
