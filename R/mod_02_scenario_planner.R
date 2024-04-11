#' scenario_planner UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT DTOutput
mod_02_scenario_planner_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      selectInput(
        ns("ics_selection"),
        "Select Integrated Care System",
        choices = ics_names,
        width = "400px"
      ),
      selectInput(
        ns("performance_metric_selection"),
        "Select performance metric to visualise",
        choices = performance_metrics(),
        width = "400px"
      ),
      plotOutput(ns("performance_plot")),
      h2("Data inputs"),
      sliderInput(
        inputId = ns("horizon_selector"),
        label = "Select number of years for planning",
        min = 1,
        max = 10,
        value = 1,
        step = 1
      ),
      h3("Scenario selector"),
      splitLayout(
        cellWidths = c("400px", "100px"),
        p("Use last known value"),
        actionButton(
          inputId = ns("last_known_value_button"),
          label = "Apply"
        )
      ),
      p("Apply a year on year percentage change to last known value"),
      splitLayout(
        cellWidths = c("400px", "100px"),
        numericInput(
          inputId = ns("percent_change_val"),
          label = "Enter percentage change (where 1 is a 1% increase each year on the previous year)",
          value = 5
        ),
        actionButton(
          inputId = ns("percent_change_button"),
          label = "Apply"
        )
      ),
      p("Apply linear trend based on previous values"),
      splitLayout(
        cellWidths = c("400px", "100px"),
        numericInput(
          inputId = ns("linear_val"),
          label = "Number of years to determine linear trend",
          value = 3,
          min = 1,
          max = 5
        ),
        actionButton(
          inputId = ns("linear_button"),
          label = "Apply"
        )
      ),
      DT::DTOutput(ns("scenario_data"))
    )
  )
}

#' scenario_planner Server Functions
#' @noRd
#' @importFrom DT datatable renderDT
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

    # pass plot to output
    output$performance_plot <- renderPlot({
      r$performance_plot
    }, res = 96)


    # calculate the scenario data if "last known value" selected
    observeEvent(
      input$last_known_value_button, {
        r$scenario_data <- scenario_inputs(
          ics_code = r$ics_cd,
          horizon = input$horizon_selector,
          scenario = "last_known_year"
        )
      })


    # calculate the scenario data if "last known value" selected
    observeEvent(
      input$percent_change_button, {
        r$scenario_data <- scenario_inputs(
          ics_code = r$ics_cd,
          horizon = input$horizon_selector,
          scenario = "percent_change",
          percent = input$percent_change_val
        )
      })

    # pass scenario data table to output
    # see this blog for editable table that stores the inputs
    # http://www.stencilled.me/post/2019-04-18-editable/
    output$scenario_data <- DT::renderDT({
      DT::datatable(
        r$scenario_data,
        editable = TRUE,
        rownames = FALSE
      )
    })

  })

}
