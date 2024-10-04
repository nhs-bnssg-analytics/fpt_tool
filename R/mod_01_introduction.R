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
            "On the <b>Scenario planner</b> tab in the <i>Make ICS and metric selection</i> area:",
            "<ol><li>Select the ICS</li>",
            "<li>Select and delete (using backspace) the performance metrics that are not of interest</li></ol>",
            "Note the pie chart updates after making the ICS selection.",
            "This informs the user how the acute metrics within the tool are calculated for each ICS.",
            "The proportions displayed in the pie chart are used as a weighting for how much each acute metric contributes to the overall ICS metric."
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
            "The bottom of the <b>Scenario planner</b> tab, in the <i>Scenario selector</i> section, has functionality to provide different future scenarios for demand and capacity.",
            "<br>First, select the number of years for planning",
            "<br>Two tabs are then available: <b>Custom</b> (the default) and <b>Template</b>.",
            "These tabs provide data points for each input metric for future years.",
            "These metrics are provided to the models, and predictions of performance in future years are presented in the <i>Performance viewer</i> section.",
            "<h4>Template scenario tab</h3>",
            "This tab has three simple options:",
            "<ol><li>Last observed value - apply the last observed value for each input metric to all subsequent years prior to modelling.</li>",
            "<li>Percentage change - apply a blanket, user-defined, percentage change to all input metrics.",
            "This can be an increase or a decrease.",
            "<li>Linear change - determine future values based on a linear extrapolation of previously observed values.",
            "The number of observed values used for the exptrapolation is determined by the user.</li></ol>",
            "Note, Input data are contstrained to between 0 and 100 if they are a proportion.",
            "Input data are also constrained so they always remain within a range of values that have previously been seen.</li>",
            "<br><br>By selecting 'Display on chart' or using the 'Apply...' buttons, the future values are calculated and passed through the models to estimate future performance values.",
            "<h4>Custom scenario tab</h3>",
            "Make use of the table of metrics at the bottom of the <i>Scenario selector</i> section.",
            "Note, the order of the metrics in the table are the order of how influential the metrics on the selected performance metrics.",
            "<ol><li>Provide the custom scenario a name.</li>",
            "<li>Select the method to pre-populate the scenario data with based on the three template scenarios described above (the default values are the 'last observed value' template scenario).",
            "Note, for the percentage change or linear options, the inputs are populated using the settings in the <b>Template tab</b>.</li>",
            "<li>There are two methods of applying a custom scenario:",
            "<ul><li>Interactive (simple scenarios): double click on the data cells in the table and interactively change their values.</li>",
            "<li>Export-import (complex scenarios):",
            "<ol type = 'i'><li>Select 'Copy' or 'CSV' at the top of the table</li>",
            "<li>Save the data in a csv file.</li>",
            "<li>Make edits to the csv file without changing column names or names of the metrics.</li>",
            "<li>Re-save the file.</li>",
            "<li>Within the tool, select 'Browse...' under 'Import csv' to import the csv file.</li>",
            "<li>Check that the values in the table have been overwritten with the values from the file.</li></ol></ul>",
            "<li>Press the green '+' symbol next to the scenario name to put the scenario through the models and add to the chart(s).</li>",
            "</li></ol>"
          )
        )
      )
    )
  )

  reporting_card <- card(
    card_header(
      "Reporting",
      class = "default-card-header"
    ),
    card_body(
      p(
        "Once all scenarios have been added to the chart(s), the 'Generate report' button (beneath the charts) allows the user to export a word document with the numbers behind the charts."
      )
    )
  )

  modification_card <- card(
    card_header(
      "Removing scenarios",
      class = "default-card-header"
    ),
    card_body(
      p(
        paste(
          "If 'Template' scenarios have been added, they can be removed by simply unchecking the box associated with them.",
          "If custom scenarios have been added, they can be removed by typing their names into the 'Scenario name' box, and clicking the red '-' symbol."
        )
      )
    )
  )
  tagList(
    bslib::page_fluid(
      metric_ics_selection_card,
      scenario_card,
      bslib::layout_columns(
        reporting_card,
        modification_card,
        col_widths = 1/2
      )
    )
  )
}

#' 01_introduction Server Functions
#'
#' @noRd
mod_01_introduction_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
