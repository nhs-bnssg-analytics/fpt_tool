#' plotting
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import ggplot2
#' @importFrom dplyr filter pull
#' @noRd
plot_performance <- function(historic_data, performance_metric) {
  ics_name <- historic_data |>
    pull(!!sym("org")) |>
    unique() |>
    ics_name_lkp()

  plot <- historic_data |>
    filter(
      !!sym("metric") %in% performance_metric
    ) |>
    ggplot(
      aes(
        x = factor(year),
        y = value
      )
    ) +
    geom_line(
      aes(
        group = value_type,
        linetype = value_type
      )
    ) +
    labs(
      title = performance_metric,
      subtitle = ics_name,
      x = "Year",
      y = "Proportion"
    ) +
    theme_bw() +
    facet_wrap(
      facets = vars(metric),
      scales = "free_y"
    )

  return(plot)
}
