#' plot a number of timeseries charts for the performance metrics that contain
#' the modelled scenarios too
#'
#' @return a facetted ggplot object
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
      y = "Proportion (%)"
    ) +
    theme_bw() +
    facet_wrap(
      facets = vars(metric),
      scales = "free_y"
    )

  return(plot)
}

#' Draw a pie chart showing the proportion of each trust that the populations of
#' the selected ICB go to
#'
#' @param ics_code character; three letter code for ICB starting with 'Q'
#'
#' @return ggplot object
#' @import ggplot2
#' @importFrom stringr str_wrap
#' @importFrom dplyr filter mutate summarise case_when
#' @importFrom scales percent
#' @importFrom ggrepel geom_text_repel
#' @noRd
plot_trust_icb_proportions <- function(ics_code) {

  df <- trust_ics_props |>
    dplyr::filter(ICB22CDH == ics_code) |>
    mutate(
      TrustName = case_when(
        proportion > 0.05 ~ TrustName,
        .default = "Other trusts"
      )
    ) |>
    summarise(
      proportion = sum(proportion),
      .by = c(TrustName, ICB22CDH)
    ) |>
    arrange(
      proportion
    ) |>
    mutate(
      TrustName = factor(TrustName, levels = TrustName)
    )


  df2 <- df |>
  mutate(
    csum = rev(cumsum(rev(proportion))),
    pos = proportion / 2 + dplyr::lead(csum, 1),
    pos = dplyr::if_else(is.na(pos), proportion / 2, pos)
  )

  p <- df |>
    ggplot(
      aes(
        x = "",
        y = proportion,
        fill = TrustName
      )
    ) +
    geom_col(
      colour = "white"
    ) +
    coord_polar(
      theta = "y",
      start = 0
    ) +
    theme_void() +
    theme(
      legend.position = "none"
    ) +
    geom_text_repel(
      data = df2,
      aes(
        y = pos,
        label = stringr::str_wrap(
          paste0(
            TrustName,
            " (",
            scales::percent(proportion, accuracy = 1),
            ")"
            ), 25)
      ),
      # position = position_stack(vjust = 0.5),
      nudge_x = 1,
      color = "black",
      show.legend = FALSE,
      size = 4
    ) +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title = "Patient admissions from residents in ICS",
      caption = "Source: NHS Acute (Hospital) Trust Catchment Populations, Office for Health Improvement and Disparities"
    )

  return(p)
}
