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
  tagList()
  fluidPage(
    fluidRow(
      column(width = 8, h2(("Background"))),
      column(width = 8, p(
        "Understanding system demand and capacity is essential for operational and strategic healthcare planning. A common approach to demand and capacity planning is spreadsheet modelling and simulating patient flow through the health system. Within each Integrated Care System (ICS) these kinds of models are possible to make, and often already exist. It is much harder to do this at a regional level because the structures and populations in each ICS are very different to one another. A tool that is developed to try and capture the intricacies and detail in one ICS is therefore likely to be unrepresentative and open to challenge if used to inform local decision making in another. Additionally, each ICS has developed their own bespoke tools to understand demand and capacity, which already supports local decision making, particularly in the acute setting."
      )),
      column(width = 8, p(
        "This tool has been developed using only publicly available data for the whole of England. These data are described in more detail on the 'Data' tab. The methods underpinning data processing and modelling can be found",
        a(tags$strong("here."), href = "https://nhs-bnssg-analytics.github.io/d_and_c/outputs/01_index.html"),
        "The analysis and modelling was done using R, and has been made publicly available",
        a(tags$strong("here."), href = "https://github.com/nhs-bnssg-analytics/d_and_c")
      )),
      column(width = 8, h2(("How to use the tool"))),
      column(width = 8, p("On the Scenario planner tab:")),
      column(width = 8, p("1. Select the ICS")),
      column(width = 8, p("2. Select and delete (using backspace) the performance metrics that are not of interest")),
      column(width = 8, p("3. Scroll down to the bottom of the page")),
      column(width = 8, p("4. Select the number of years into the future for modelling")),
      column(width = 8, p("5. In the Template scenarios tab, deselect any scenarios that are not of interest")),
      column(width = 8, p("6. If the percent change or linear scenarios are retained, update their inputs as desired")),
      column(width = 8, p("7. Select the custom scenario tab")),
      column(width = 8, p("8. The custom scenario table is pre-populated from the 'last observed value' scenario. To update the whole table from another scenario, click the appropriate button at the top of the tab")),
      column(width = 8, p("9. Any value in the table can be changed by double clicking in a cell and typing a new value, followed by the tab key")),
      column(width = 8, p("10. Once all fo the new values have been entered, click the 'Update predictions' button below the chart and see the effect of the scenarios to future years")),
      column(width = 8, h2("Questions this can support")),
      column(width = 8, p(
        "This tool should be used to support and build cases for medium term investment. It can be used in different ways. For example, with a particular performance metric in mind, it can be used to see what mix of investment over time can help improve performance. Separately, it can be used to understand how investment in a particular capacity input, or population health management programme which will result in changes to demand, will affect multiple performance metrics."
      )),
      column(width = 8, h2("Limitations")),
      column(width = 8, p("Missing variables can lead to relationships that aren't causal. Estimations applied to actue data. ")),
      column(width = 8, h2("Contact us")),
      column(width = 8, p(
        "If you have any feedback or questions, please get in touch at",
        a(tags$strong("sebastian.fox3@nhs.net"),
          href = "mailto:sebastian.fox3@nhs.net?subject=Demand Capacity planning tool"
        )
      ))
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
