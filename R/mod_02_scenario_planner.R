#' scenario_planner UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib navset_card_tab input_task_button card card_header card_body layout_column_wrap
#' @importFrom DT DTOutput
mod_02_scenario_planner_ui <- function(id){
  ns <- NS(id)

  # scenario cards
  last_known_card <- card(
    fill = FALSE,
    class = "scenario-card",
    card_header(
      "Last observed value",
      class = "scenario-card-header"
    ),
    checkboxInput(
      inputId = ns("display_last_known"),
      label = "Display on chart",
      value = TRUE
    )
  )

  percent_card <- card(
    class = "scenario-card",
    card_header(
      "Percentage change",
      class = "scenario-card-header"
    ),
    checkboxInput(
      inputId = ns("display_percent"),
      label = "Display on chart",
      value = TRUE
    ),
    card_body(
      p("Apply a year on year percentage change to the last observed value for each metric to populate future years for the custom scenario."),
      p("Note, for metrics that are a proportion, the values get constrained to values between 0 and 1.")
    ),
    card_body(
      class = "card-body-input",
      numericInput(
        inputId = ns("percent_change_val"),
        label = "Enter percentage change (where 1 is a 1% increase each year on the previous year)",
        value = 5,
        width = "100%"
      )
    ),
    card_body(
      min_height = "100px",
      bslib::input_task_button(
        id = ns("apply_percent_change_button"),
        label = "Apply percentage change scenario to performance chart",
        label_busy = "Updating performance chart...",
        type = "secondary",
        width = "75%"
      )
    )
  )

  linear_card <- card(
    class = "scenario-card",
    card_header(
      "Linear change",
      class = "scenario-card-header"
    ),
    checkboxInput(
      inputId = ns("display_linear"),
      label = "Display on chart",
      value = TRUE
    ),
    card_body(
      p("Extrapolate the last observed values for each metric to populate future years for the custom scenario."),
      p("Note, for metrics that are a proportion, the values get constrained to values between 0 and 1.")
    ),
    card_body(
      class = "card-body-input",
      numericInput(
        inputId = ns("linear_val"),
        label = "Enter the number of years to use to determine the linear trend",
        value = 3,
        min = 1,
        max = 5,
        width = "75%"
      )
    ),
    card_body(
      min_height = "100px",
      bslib::input_task_button(
        id = ns("apply_linear_button"),
        label = "Apply linear scenario to performance chart",
        label_busy = "Updating performance chart...",
        type = "secondary"
      )
    )
  )

  custom_template_card <- card(
    card_header(
      "Populate a custom scenario",
      class = "scenario-card-header"
    ),
    checkboxInput(
      inputId = ns("display_custom"),
      label = "Display on chart",
      value = TRUE
    ),
    card_body(
      textInput(
        inputId = ns("custom_name"),
        label = "Enter scenario name",
        value = "Custom scenario"
      )
    ),
    card_body(
      p("Select an option to pre-populate your custom scenario data below.")
    ),
    layout_column_wrap(
      card(
        class = "button-card",
        card_body(
          min_height = "100px",
          bslib::input_task_button(
            id = ns("last_known_value_button"),
            label = "Last observed value",
            label_busy = "Updating custom scenario...",
            type = "secondary"
          )
        )
      ),
      card(
        class = "button-card",
        card_body(
          min_height = "100px",
          bslib::input_task_button(
            id = ns("percent_change_button"),
            label = "Percentage change",
            label_busy = "Updating custom scenario...",
            type = "secondary"
          )
        )
      ),
      card(
        class = "button-card",
        card_body(
          min_height = "100px",
          bslib::input_task_button(
            id = ns("linear_button"),
            label = "Linear extrapolation",
            label_busy = "Updating custom scenario...",
            type = "secondary"
          )
        )
      )
    ),
    card(
      card_body(
        radioButtons(
          inputId = ns("custom_display"),
          label = "Which demand and capacity variables do you want to display below?",
          width = "100%",
          choices = c(
            "All" = "all",
            "Most important" = "important",
            "Top 15 important" = "top_n"
          ),
          selected = "top_n"
        )
      ),
      card_body(
        min_height = "50vh",
        max_height = "80vh",
        DT::DTOutput(ns("scenario_data_custom"))
      )
    )
  )

  # start populating the ui of the shiny app
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
      input_task_button(
        id = ns("model_scenario_button"),
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
      # begin the section for selecting the scenario inputs
      h2("Scenario selector"),
      bslib::navset_card_tab(
        full_screen = TRUE,
        bslib::nav_panel(
          title = "Template scenarios",
          layout_column_wrap(
            width = "400px",
            # height = 500,
            last_known_card,
            percent_card,
            linear_card
          )
        ),
        bslib::nav_panel(
          title = "Custom scenario",
          custom_template_card
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
    model_outputs <- load_model_object()

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

        # which scenarios have a checkbox saying to include in the chart
        display_scenarios <- c(
          last_known = input$display_last_known,
          percent = input$display_percent,
          linear = input$display_linear,
          custom = input$display_custom
        )

        update_predictions(
          prediction_custom_scenario = input$custom_name,
          model_outputs = model_outputs |>
            lapply(
              function(x) x[["wf"]]
            ),
          display_scenarios = display_scenarios,
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

        update_custom_tables(
          input_table = last_known,
          model_permutation_importance = model_outputs |>
            lapply(
              function(x) x[["perm_imp"]]
            ),
          performance_metrics = input$performance_metric_selection,
          table_options = input$custom_display,
          r = r
        )


        # print("last_known_year")
      })


    # calculate the scenario data if "percent change" selected
    observeEvent(
      input$percent_change_button, {
        percent_change <- scenario_inputs(
          ics_code = r$ics_cd,
          horizon = input$horizon_selector,
          scenario = "percent_change",
          percent = input$percent_change_val
        )

        update_custom_tables(
          input_table = percent_change,
          model_permutation_importance = model_outputs |>
            lapply(
              function(x) x[["perm_imp"]]
            ),
          performance_metrics = input$performance_metric_selection,
          table_options = input$custom_display,
          r = r
        )
        # print("percent_change")
      })

    # calculate the scenario data if "linear" selected
    observeEvent(
      input$linear_button, {
        linear_change <- scenario_inputs(
          ics_code = r$ics_cd,
          horizon = input$horizon_selector,
          scenario = "linear",
          linear_years = input$linear_val
        )

        update_custom_tables(
          input_table = linear_change,
          model_permutation_importance = model_outputs |>
            lapply(
              function(x) x[["perm_imp"]]
            ),
          performance_metrics = input$performance_metric_selection,
          table_options = input$custom_display,
          r = r
        )
        # print("linear")
      })

    # update the performance chart when changes to the linear scenario is
    # applied
    observeEvent(
      input$apply_linear_button, {

        linear_change <- scenario_inputs(
          ics_code = r$ics_cd,
          horizon = input$horizon_selector,
          scenario = "linear",
          linear_years = input$linear_val
        )

        r$scenario_data$linear <- linear_change

      }
    )

    # update the performance chart when changes to the percent change scenario
    # is applied
    observeEvent(
      input$apply_percent_change_button, {

        percent_change <- scenario_inputs(
          ics_code = r$ics_cd,
          horizon = input$horizon_selector,
          scenario = "percent",
          percent = input$percent_change_val
        )

        r$scenario_data$percent <- percent_change

      }
    )

    # pass scenario data table to output
    output$scenario_data_custom <- DT::renderDT({

      update_custom_tables(
        input_table = r$scenario_data$custom,
        model_permutation_importance = model_outputs |>
          lapply(
            function(x) x[["perm_imp"]]
          ),
        performance_metrics = input$performance_metric_selection,
        table_options = input$custom_display,
        r = r
      )

      numeric_cols <- setdiff(
        names(r$scenario_data$custom_display),
        c("metric", "domain")
      )

      DT::datatable(
        r$scenario_data$custom_display,
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
      r$scenario_data$custom_display <<- DT::editData(
        data = r$scenario_data$custom_display,
        info = edited_cell_info,
        proxy = ns("scenario_data_custom"),
        resetPaging = TRUE # testing this because currently
      )

      custom_table <- bind_rows(
        r$scenario_data$custom_display,
        r$scenario_data$custom_stored
      )

      r$scenario_data$custom <<- custom_table

    })

    # apply scenario through model to predict outcome
    observeEvent(
      c(input$display_last_known,
        input$display_linear,
        input$display_percent,
        input$display_custom,
        input$model_scenario_button,
        input$apply_percent_change_button,
        input$apply_linear_button), {

        display_scenarios <- c(
          last_known = input$display_last_known,
          percent = input$display_percent,
          linear = input$display_linear,
          custom = input$display_custom
        )

        update_predictions(
          prediction_custom_scenario = input$custom_name,
          model_outputs = model_outputs |>
            lapply(
              function(x) x[["wf"]]
            ),
          display_scenarios = display_scenarios,
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

