#' scenario_planner UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib navset_card_tab
#' @importFrom DT DTOutput
mod_02_scenario_planner_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      selectInput(
        ns("ics_selection"),
        "Select Integrated Care System",
        choices = ics_names,
        selected = ics_names[1],
        width = "800px"
      ),
      selectInput(
        ns("performance_metric_selection"),
        "Select performance metric to visualise",
        choices = performance_metrics(),
        multiple = TRUE,
        selected = performance_metrics(),
        width = "400px"
      ),
      plotOutput(ns("performance_plot")),
      actionButton(
        inputId = ns("model_scenario_button"),
        label = "Update predictions",
        width = "300px"
      ),
      h2("Data inputs"),
      sliderInput(
        inputId = ns("horizon_selector"),
        label = "Select number of years for planning",
        min = 1,
        max = 10,
        value = 5,
        step = 1
      ),
      h2("Scenario selector"),
      bslib::navset_card_tab(
        nav_panel(
          title = "Last known value",
          p("Use last known value"),
          actionButton(
            inputId = ns("last_known_value_button"),
            label = "Update scenario",
            width = "300px"
          )
        ),
        nav_panel(
          title = "Percentage change",
          p("Apply a year on year percentage change to last known value"),
          numericInput(
            inputId = ns("percent_change_val"),
            label = "Enter percentage change (where 1 is a 1% increase each year on the previous year)",
            value = 5
          ),
          actionButton(
            inputId = ns("percent_change_button"),
            label = "Update scenario",
            width = "300px"
          )
        ),
        nav_panel(
          title = "Linear change",
          p("Apply linear trend based on previous values"),
          numericInput(
            inputId = ns("linear_val"),
            label = "Number of years to determine linear trend",
            value = 3,
            min = 1,
            max = 5
          ),
          actionButton(
            inputId = ns("linear_button"),
            label = "Update scenario",
            width = "300px"
          )
        ),
        nav_panel(
          title = "Custom scenario",
          p("Enter custom values for scenario"),
          textInput(
            inputId = ns("custom_name"),
            label = "Enter scenario name",
            value = "Custom scenario"
          ),
          DT::DTOutput(ns("scenario_data_custom"))
        )
      )
    )
  )
}

#' scenario_planner Server Functions
#' @noRd
#' @importFrom DT datatable renderDT formatRound editData
#' @importFrom dplyr tibble distinct anti_join join_by
#' @importFrom purrr map_df
#' @importFrom rlang sym
#' @param r a `reactiveValues()` list with ics_cd (string, 3 letter code for
#'   ics), ics_data (tibble containing observed data for performance metrics for
#'   the selected ICS), performance_plot (ggplot time series of observed and
#'   predicted values for the performance metrics by ICS), scenario_data (list
#'   for 4 items which are each predicted values in a tibble for the performance
#'   metrics for different scenarios: last_known, percent, linear, custom)

mod_02_scenario_planner_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # load the model outputs
    model_outputs <- readRDS("C:/Users/Sebastian.Fox/Documents/R/Play/d_and_c/outputs/model_objects/wfs.rds")

    observeEvent(
      c(input$ics_selection,
        input$performance_metric_selection
      ), {
        # log the ICS code selected in the r database
        r$ics_cd <- ics_code_lkp(input$ics_selection)

        # make the historic performance data for the ICS available in the r database
        r$ics_data <- ics_data(
          ics_code = r$ics_cd,
          domain_type = "Performance"
        )

        default_data <- reset_scenarios(
          ics_cd = r$ics_cd,
          horizon = input$horizon_selector,
          percent = input$percent_change_val,
          linear_years = input$linear_val
        )

        for (nm in names(default_data)) {
          r$scenario_data[[nm]] <- default_data[[nm]]
        }

        update_predictions(
          prediction_custom_scenario = input$custom_name,
          r = r
        )

        # draw chart of historic data when ICS selection changes
        r$performance_plot <- plot_performance(
          historic_data = bind_rows(
            r$ics_data,
            r$predictions
          ),
          performance_metric = input$performance_metric_selection
        )
    })

    # pass plot to output
    output$performance_plot <- renderPlot({
      r$performance_plot
    }, res = 96)


    # scenario data -----------------------------------------------------------

    # calculate the scenario data if "last known value" selected
    observeEvent(
      input$last_known_value_button, {
        last_known <- scenario_inputs(
          ics_code = r$ics_cd,
          horizon = input$horizon_selector,
          scenario = "last_known_year"
        )
        r$scenario_data$last_known <- last_known
        r$scenario_data$custom <- last_known
        # print("last_known_year")
      })


    # calculate the scenario data if "percent change" selected
    observeEvent(
      input$percent_change_button, {
        r$scenario_data$percent <- scenario_inputs(
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
        r$scenario_data$linear <- scenario_inputs(
          ics_code = r$ics_cd,
          horizon = input$horizon_selector,
          scenario = "linear",
          linear_years = input$linear_val
        )
        # print("linear")
      })


    # pass scenario data table to output
    output$scenario_data_custom <- DT::renderDT({
      numeric_cols <- setdiff(
        names(r$scenario_data$custom),
        c("metric", "domain")
      )
      DT::datatable(
        r$scenario_data$custom,
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
    observeEvent(input$scenario_data_custom_cell_edit, {
      edited_cell_info <- input$scenario_data_custom_cell_edit |>
        mutate(col = col + 1) # this is because there is an offset as rownames = FALSE

      # str(edited_cell_info)
      r$scenario_data$custom <<- DT::editData(
        data = r$scenario_data$custom,
        info = edited_cell_info,
        proxy = ns("scenario_data_custom")
      )

    })

    # apply scenario through model to predict outcome
    observeEvent(input$model_scenario_button, {
      update_predictions(
        prediction_custom_scenario = input$custom_name,
        r = r
      )

      r$performance_plot <- plot_performance(
        historic_data = bind_rows(
          r$ics_data,
          r$predictions
        ),
        performance_metric = input$performance_metric_selection
      )

      output$performance_plot <- renderPlot({
        r$performance_plot
      }, res = 96)
    })

  })

}

