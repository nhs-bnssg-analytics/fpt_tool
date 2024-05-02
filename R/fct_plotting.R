#' plotting
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import ggplot2
#' @importFrom dplyr filter pull
#' @importFrom tidyr separate_wider_delim replace_na
#' @importFrom stringr str_to_sentence
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
    tidyr::separate_wider_delim(
      cols = !!sym("value_type"),
      names = c("value_type", "scenario"),
      delim = " - ",
      too_few = "align_start"
    ) |>
    mutate(
      scenario = replace_na(scenario, "Observed"),
      scenario = stringr::str_to_sentence(scenario)
    )

  known_scenarios <- c("Observed",
                       "Last known value",
                       "Percent change",
                       "Linear extrapolation")

  final_scenario <- plot |>
    pull(scenario) |>
    unique() |>
    setdiff(known_scenarios)

  known_scenarios <- factor(
    c(known_scenarios, final_scenario),
    levels = c(known_scenarios, final_scenario)
  )

  scenario_colours <- setNames(
    c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442"),
    nm = known_scenarios
  )

  plot <- plot |>
    ggplot(
      aes(
        x = factor(year),
        y = value
      )
    ) +
    geom_line(
      aes(
        group = scenario,
        linetype = value_type,
        colour = scenario
      ),
      show.legend = FALSE
    ) +
    geom_point(
      aes(
        colour = scenario
      )
    ) +
    scale_colour_manual(
      name = "Scenario",
      values = scenario_colours,
      breaks = known_scenarios
    ) +
    labs(
      title = paste(
        "Observed and predicted performance metrics for",
        ics_name
      ),
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
