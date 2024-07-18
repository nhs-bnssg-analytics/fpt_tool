#' 01_introduction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_introduction_ui <- function(id) {
  ns <- NS(id)

  metric_ics_selection_card <- card(
    card_header(
      "Selecting the ICS and metric",
      class = "default-card-header"
    ),
    card_body(
      p(
        HTML(
          paste(
            "<ol><li>Select the ICS</li>",
            "<li>Select and delete (using backspace) the performance metrics that are not of interest</li></ol>",
            "Note the pie chart updates after making the ICS selection.",
            "This informs the user how the acute metrics within the tool are calculated for each ICS.",
            "For example, the proportions displayed in the pie chart are used as a weighting for how much each acute metric contributes to the overall ICS metric."
          )
        )
      )
    )
  )

  scenario_card <- card(
    card_header(
      "Providing future scenario inputs",
      class = "default-card-header"
    ),
    card_body(
      p(
        HTML(
          paste(
            "The bottom of the scenario selector tab provides functionality to provide different future scenarios for demand and capacity.",
            "<ul><li>Select the number of years for planning</li></ul>",
            "Two tabs are then available: 'Custom' (the default) and 'Template'.",
            "These tabs provide data points for each input metric for future years.",
            "These metrics will get provided to the models, and predictions of performance in those future years are presented above.",
            "<h5>Template scenario tab</h3>",
            "This tab has three simple options:",
            "<ol><li>Last observed value - apply the last observed value for each input metric to all subsequent years prior to modelling.</li>",
            "<li>Percentage change - apply a blanket percentage change to all input metrics.",
            "This can be an increase or a decrease and the percentage change is supplied by the user.",
            "Input data are contstrained to between 0 and 100 if they are a proportion.",
            "Input data are also constrained so they always remain within a range of values that have previously been seen.</li>",
            "<li>Linear change - determine future values based on a linear extrapolation of previously observed values.",
            "The number of observed values used for the exptrapolation is determined by the user.</li></ol>",
            "By selecting 'Display on chart' or using the 'Apply...' buttons, the future values are calculated and then passed through the models to estimate future performance values.",
            "<h5>Custom scenario tab</h3>",
            "Make use of the table of metrics at the bottom of the 'Scenario selector' box.",
            "Note, the order of the metrics in the table are the order of how influential the metrics on the selected performance metrics.",
            "<ol><li>Provide the custom scenario a name.</li>",
            "<li>Select the method to pre-population the scenario data with base on the three template scenarios described above (the default values are the 'last observed value' template scenario).</li>",
            "<li>Input data:",
            "<ul><li>Interactive (simple scenarios): double click on the data cells in the table and interactively change their values.</li>",
            "<li>Export-import (complex scenarios):",
            "<ol type = 'i'><li>click 'Download custom input data' and save the csv file.</li>",
            "<li>Make edits to the csv file without changing column names or names of the metrics.</li>",
            "<li>Once complete, re-save the file.</li>",
            "<li>Within the tool, select 'Browse...' to allow you to select the csv file.</li>",
            "<li>Check that the values in the table have been overwritten with the values from the file.</li></ol></ul>",
            "</li></ol>"
          )
        )
      )
    )
  )

  tagList(
    bslib::page_fluid(
      metric_ics_selection_card,
      scenario_card
    )
  )
  #       column(
  #         width = 8,
  #         p(
  #           HTML(
  #             paste(
  #               "On the Scenario planner tab:",
  #               "<ol><li>Select the ICS</li>",
  #               "<li>Select and delete (using backspace) the performance metrics that are not of interest</li>",
  #               "<li>Scroll down to the bottom of the page</li>",
  #               "<li>Select the number of years into the future for modelling</li>",
  #               "<li>In the Template scenarios tab, deselect any scenarios that are not of interest</li>",
  #               "<li>If the percent change or linear scenarios are retained, update their inputs as desired</li>",
  #               "<li>Select the custom scenario tab</li>",
  #               "<li>The custom scenario table is pre-populated from the 'last observed value' scenario. To update the whole table from another scenario, click the appropriate button at the top of the tab</li>",
  #               "<li>Any value in the table can be changed by double clicking in a cell and typing a new value, followed by the tab key</li>",
  #               "<li>Once all of the new values have been entered, click the 'Update predictions' button below the chart and see the effect of the scenarios to future years</li></ol>"
  #             )
  #           )
  #         )
  #       )
  #     )
  #   )
  # )

}

#' 01_introduction Server Functions
#'
#' @noRd
mod_01_introduction_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
