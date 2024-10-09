#' plot a number of timeseries charts for the performance metrics that contain
#' the modelled scenarios too
#'
#' @return a facetted ggplot object
#' @import ggplot2
#' @importFrom dplyr filter pull
#' @importFrom tidyr separate_wider_delim replace_na
#' @importFrom stringr str_to_sentence
#' @param historic_data tibble with columns org, metric, year, value_type (which
#'   is the scenario name) and value
#' @noRd
plot_performance <- function(historic_data, performance_metric) {
  ics_name <- historic_data |>
    pull(!!sym("org")) |>
    unique() |>
    ics_name_lkp()

  plot <- historic_data |>
    filter(
      !!sym("metric") %in% performance_metric,
      !!sym("year") >= 2016
    ) |>
    tidyr::separate_wider_delim(
      cols = !!sym("value_type"),
      names = c("value_type", "scenario"),
      delim = " - ",
      too_few = "align_start"
    ) |>
    mutate(
      scenario = replace_na(!!sym("scenario"), "Observed"),
      scenario = stringr::str_to_sentence(!!sym("scenario")),
      metric = str_wrap(!!sym("metric"), 30)
    )

  known_scenarios <- c("Observed",
                       "Last known value",
                       "Percent change",
                       "Linear extrapolation")

  final_scenario <- plot |>
    pull(!!sym("scenario")) |>
    unique() |>
    setdiff(known_scenarios)

  known_scenarios <- factor(
    c(known_scenarios, final_scenario),
    levels = c(known_scenarios, final_scenario)
  )

  plot <- plot |>
    ggplot(
      aes(
        x = factor(!!sym("year")),
        y = !!sym("value")
      )
    ) +
    geom_line(
      aes(
        group = !!sym("scenario"),
        linetype = !!sym("value_type"),
        colour = !!sym("scenario")
      ),
      show.legend = FALSE
    ) +
    geom_point(
      aes(
        colour = !!sym("scenario")
      )
    ) +
    scale_colour_viridis_d(
      name = "Scenario",
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
      facets = vars(!!sym("metric")),
      scales = "free_y",
      ncol = 2
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
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang sym
#' @noRd
plot_trust_icb_proportions <- function(ics_code) {

  df <- trust_ics_props |>
    dplyr::filter(!!sym("ICB22CDH") == ics_code) |>
    mutate(
      TrustName = case_when(
        !!sym("proportion") > 0.05 ~ !!sym("TrustName"),
        .default = "Other trusts"
      )
    ) |>
    summarise(
      proportion = sum(!!sym("proportion")),
      .by = c(!!sym("TrustName"), !!sym("ICB22CDH"))
    ) |>
    arrange(
      !!sym("proportion")
    ) |>
    mutate(
      TrustName = factor(!!sym("TrustName"), levels = !!sym("TrustName"))
    )


  df2 <- df |>
  mutate(
    csum = rev(cumsum(rev(!!sym("proportion")))),
    pos = !!sym("proportion") / 2 + dplyr::lead(!!sym("csum"), 1),
    pos = dplyr::if_else(is.na(!!sym("pos")), !!sym("proportion") / 2, !!sym("pos"))
  )

  p <- df |>
    ggplot(
      aes(
        x = "",
        y = !!sym("proportion"),
        fill = !!sym("TrustName")
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
        y = !!sym("pos"),
        label = stringr::str_wrap(
          paste0(
            !!sym("TrustName"),
            " (",
            # scales::percent(!!sym("proportion"), accuracy = 1),
            formatC(100 * !!sym("proportion"), digits = 1, format = "f"),
            "%)"
            ), 25)
      ),
      nudge_x = 1,
      color = "black",
      show.legend = FALSE,
      size = 4,
      seed = 123
    ) +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title = "Estimated patient admissions from ICS residents",
      subtitle = "Based on all admissions between 2018/19 and 2020/21",
      caption = "Source: NHS Acute (Hospital) Trust Catchment Populations,\nOffice for Health Improvement and Disparities"
    )

  return(p)
}

#' @importFrom rlang sym
#' @import ggplot2
plot_hold_message <- function() {
  p <- tibble::tibble(
    x = 1,
    y = 1,
    text = stringr::str_wrap(
      "To display future performance, please make area and metric selections and then either add a template scenario or build a custom scenario before adding it to the chart with the green plus button",
      80
    )
  ) |>
    ggplot(
      aes(
        x = !!sym("x"),
        y = !!sym("y")
      )
    ) +
    geom_text(
      aes(
        label = !!sym("text")
      )
    ) +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      panel.border = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 1
      )
    )

  return(p)
}
