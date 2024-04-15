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
      DT::DTOutput(ns("scenario_data_out")),
      actionButton(
        inputId = ns("model_scenario_button"),
        label = "Model scenario"
      ),
      tableOutput(ns("predictions"))
    )
  )
}

#' scenario_planner Server Functions
#' @noRd
#' @importFrom DT datatable renderDT formatRound editData
#' @importFrom dplyr tibble
mod_02_scenario_planner_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

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


    # scenario data -----------------------------------------------------------
    # default at start up
    r$scenario_data <- reactiveValues(
      data = tibble(
        metric = character(),
        domain = character()
      )
    )

    # calculate the scenario data if "last known value" selected
    observeEvent(
      input$last_known_value_button, {
        r$scenario_data$data <- scenario_inputs(
          ics_code = r$ics_cd,
          horizon = input$horizon_selector,
          scenario = "last_known_year"
        )
        # print("last_known_year")
      })


    # calculate the scenario data if "percent change" selected
    observeEvent(
      input$percent_change_button, {
        r$scenario_data$data <- scenario_inputs(
          ics_code = r$ics_cd,
          horizon = input$horizon_selector,
          scenario = "percent_change",
          percent = input$percent_change_val
        )
        # print("percent_change")
      })

    # calculate the scenario data if "linear" selected
    observeEvent(
      input$linear_button, {
        r$scenario_data$data <- scenario_inputs(
          ics_code = r$ics_cd,
          horizon = input$horizon_selector,
          scenario = "linear",
          linear_years = input$linear_val
        )
        # print("linear")
      })


    # pass scenario data table to output
    output$scenario_data_out <- DT::renderDT({
      numeric_cols <- setdiff(
        names(r$scenario_data$data),
        c("metric", "domain")
      )
      DT::datatable(
        r$scenario_data$data,
        rownames = FALSE,
        editable = list(
          target = "cell",
          disable = list(columns = c(0, 1)), # disable editing metric and domain fields
          numeric = "all" # allow only numeric values
        ),
        selection = "none", # don't need to be able to select rows
        colnames = c(
          "Metric" = "metric",
          "Domain" = "domain"
        ),
        options = list(
          pageLength = 25,
          autoWidth = TRUE
        )
      ) |>
        DT::formatRound(
          columns = numeric_cols
        )
    })

    # store editted scenario_data
    # https://rstudio.github.io/DT/shiny.html
    # https://yihui.shinyapps.io/DT-edit/
    observeEvent(input$scenario_data_out_cell_edit, {
      edited_cell_info <- input$scenario_data_out_cell_edit |>
        mutate(col = col + 1) # this is because there is an offset because rownames = FALSE

      # str(edited_cell_info)
      r$scenario_data$data <<- DT::editData(
        data = r$scenario_data$data,
        info = edited_cell_info,
        proxy = ns("scenario_data_out")
      )

    })

    # apply scenario through model to predict outcome
    observeEvent(input$model_scenario_button, {
      r$predictions <- model_scenario_data(
        scenario_data = r$scenario_data$data,
        ics_code = r$ics_cd,
        target_metric = input$performance_metric_selection
      )

      r$performance_plot <- plot_performance(
        historic_data = bind_rows(
          r$ics_data,
          r$predictions
        ),
        performance_metric = input$performance_metric_selection
      )

      output$predictions <- renderTable(r$predictions)
      output$performance_plot <- renderPlot({
        r$performance_plot
      }, res = 96)
    })

  })

}

