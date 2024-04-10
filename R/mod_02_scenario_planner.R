#' scenario_planner UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_02_scenario_planner_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      selectInput(
        ns("ics_selection"),
        "Select Integrated Care System",
        choices = ics_names
      ),
      selectInput(
        ns("performance_metric_selection"),
        "Select performance metric to visualise",
        choices = performance_metrics()
      ),
      plotOutput(ns("performance_plot"))
    )
  )
}

#' scenario_planner Server Functions
#' @noRd
mod_02_scenario_planner_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    # ns <- session$ns

    observeEvent(
      input$ics_selection, {
      r$ics_cd <- ics_code_lkp(input$ics_selection)
      r$ics_data <- ics_data(
        ics_code = r$ics_cd,
        domain_type = "Performance"
      )
    })

    # draw chart of historic data when ICS selection changes
    observeEvent(
      input$ics_selection, {
        r$performance_plot <- plot_performance(
          historic_data = r$ics_data,
          performance_metric = input$performance_metric_selection
        )
      })

    observeEvent(
      input$performance_metric_selection, {
      r$ics_cd <- ics_code_lkp(input$ics_selection)
      r$ics_data <- ics_data(
        ics_code = r$ics_cd,
        domain_type = "Performance"
      )
    })

    # draw chart of historic data when metric selection changes
    observeEvent(
      input$performance_metric_selection, {
      r$performance_plot <- plot_performance(
        historic_data = r$ics_data,
        performance_metric = input$performance_metric_selection
      )
    })

    # record plot to global r database
    output$performance_plot <- renderPlot({
      r$performance_plot
    }, res = 96)



  })

}
