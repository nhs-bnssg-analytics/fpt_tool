#' scenario_planner UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList downloadButton
#' @importFrom bslib navset_card_tab input_task_button card card_header
#'   card_body layout_column_wrap layout_sidebar sidebar
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
      value = FALSE
    ),
    card_body(
      p("Apply the last observed value for all input metrics to future years.")
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
      value = FALSE
    ),
    card_body(
      p("Apply a year on year percentage change to the last observed value for each metric to populate future years for the custom scenario."),
      p("Note, for metrics that are a proportion, the values get constrained to values between 0 and 100.")
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
      value = FALSE
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
    card_body(
      layout_column_wrap(
        width = NULL,
        style = "grid-template-columns: 300px 50px 50px;",
        textInput(
          inputId = ns("custom_name"),
          label = NULL,
          placeholder = "Enter scenario name"
        ),
        span(
          input_task_button(
            id = ns("btn_add_scenario_prediction"),
            label = NULL,
            icon = icon("plus", style = "color: white;"),
            style = "background-color: green; border: none; padding: 5px 0 5px 0; width: 40px",
            label_busy = "Predicting...",
            type = "secondary"
          ),
          class = "tooltiptext",
          title = "Make predictions and add to chart"
        ),
        span(
          input_task_button(
            id = ns("btn_remove_scenario_prediction"),
            label = NULL,
            icon = icon("minus", style = "color: white;"),
            style = "background-color: red; border: none; padding: 5px 0 5px 0; width: 40px",
            label_busy = "Removing...",
            type = "secondary"
          ),
          class = "tooltiptext",
          title = "Remove scenario from chart"
        )

          # shinyBS::bsTooltip(
          #   id = ns("btn_add_scenario_prediction"),
          #   title = "Make predictions and add to chart",
          #   placement = "top",
          #   trigger = "hover",
          #   options = list(container = "body")
          # ),
          # shinyBS::tipify(
          #   ,
          #   "Testing"
          # )
          # ,
          # shinyBS::bsTooltip(
          #   id = "btn_remove_scenario_prediction",
          #   title = "Remove scenarios from chart",
          #   placement = "top",
          #   trigger = "hover",
          #   options = list(container = "body")
          # )
        # )




          # shinyBS::bsTooltip(
          #   id = ns("btn_add_scenario_prediction"),
          #   title = "Make predictions and add to chart",
          #   placement = "top",
          #   trigger = "hover"
          # )#,
          # shinyBS::bsTooltip(
          #   id = ns("btn_remove_scenario_prediction"),
          #   title = "Remove scenarios from chart",
          #   placement = "top",
          #   trigger = "hover"
          # )
        # )#,
        # actionButton(
        #   inputId = ns("btn_add_scenario_prediction"),
        #   label = NULL,
        #   icon = icon("plus", style = "color: green;")
        # ),
        # actionButton(
        #   inputId = ns("btn_remove_scenario_prediction"),
        #   label = NULL,
        #   icon = icon("minus", style = "color: red;"))
      )
      # tooltip(
      #   span(
      #     "This text does trigger",
      #     bs_icon("info-circle")
      #   ),
      #   "Tooltip message",
      #   placement = "bottom"
      # )
    ),
    card_body(
      p("Select an option to pre-populate your custom scenario data below.")
    ),
    layout_column_wrap(
      width = 0.2,
      bslib::input_task_button(
        id = ns("last_known_value_button"),
        label = "Last observed value",
        label_busy = "Updating custom scenario...",
        type = "secondary"
      ),
      bslib::input_task_button(
        id = ns("percent_change_button"),
        label = "Percentage change",
        label_busy = "Updating custom scenario...",
        type = "secondary"
      ),
      bslib::input_task_button(
        id = ns("linear_button"),
        label = "Linear extrapolation",
        label_busy = "Updating custom scenario...",
        type = "secondary"
      )
    ),
    radioButtons(
      inputId = ns("custom_display"),
      label = "Which demand and capacity variables do you want to display below?",
      width = "100%",
      choices = c(
        "All" = "all",
        "Top 15 important" = "top_n"
      ),
      selected = "top_n"
    ),
    downloadButton(
      outputId = ns("download_custom_scenario_inputs"),
      label = "Download custom input data",
      style = "width:25%"
    ),
    fileInput(
      ns("custom_scenario_file"),
      "Choose custom scenario CSV file",
      accept = c("text/csv",
                 "text/comma-separated-values,
                       .csv"),
      width = "300px"
    ),
    # layout_column_wrap(
    #   width = 0.25,
    #   bslib::input_task_button(
    #     id = ns("btn_add_scenario_prediction"),
    #     icon = icon("plus", style = "color: green;"),
    #     label = "Make predictions and add to chart",
    #     label_busy = "Predicting...",
    #     type = "secondary"
    #   ),
    #   bslib::input_task_button(
    #     id = ns("btn_remove_scenario_prediction"),
    #     label = "Remove scenarios from chart",
    #     label_busy = "Removing...",
    #     type = "secondary"
    #   )
    # ),
    card_body(
      min_height = "50vh",
      max_height = "80vh",
      DT::DTOutput(ns("scenario_data_custom"))
    )
  )

  selector_card <- card(
    card_header(
      "Make ICS and metric selection",
      class = "default-card-header"
    ),
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
    plotOutput(
      ns("trust_icb_plot")
    )
  )

  performance_card <- card(
    card_header(
      "Performance viewer",
      class = "default-card-header"
    ),
    plotOutput(
      ns("performance_plot"),
      height = '60vh'
    ),
    downloadButton(
      ns("report_btn"),
      "Generate report",
      style = "width:25%;"
    )
  )

  scenario_card <- card(
    card_header(
      "Scenario selector",
      class = "default-card-header"
    ),
    sliderInput(
      inputId = ns("horizon_selector"),
      label = "Select number of years for planning",
      min = 1,
      max = 10,
      value = 5,
      step = 1
    ),
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel(
        title = "Custom scenario",
        custom_template_card
      ),
      bslib::nav_panel(
        title = "Template scenarios",
        layout_column_wrap(
          width = "400px",
          # height = 500,
          last_known_card,
          percent_card,
          linear_card
        )
      )
    )

  )

  tagList(
    bslib::page_fluid(
      bslib::layout_sidebar(
        sidebar = sidebar(
          selector_card,
          open = TRUE,
          width = '25%'
        ),
        performance_card,
        # begin the section for selecting the scenario inputs
        scenario_card
      )
    )
  )
}

#' scenario_planner Server Functions
#' @noRd
#' @importFrom DT datatable renderDT formatRound editData JS
#' @importFrom dplyr tibble distinct anti_join join_by as_tibble
#' @importFrom purrr map_df
#' @importFrom rlang sym
#' @importFrom shiny downloadHandler
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

        # remove all previously predicted data
        r$predictions <- NULL

        default_data <- reset_scenarios(
          ics_cd = r$ics_cd,
          horizon = input$horizon_selector,
          percent = input$percent_change_val,
          linear_years = input$linear_val
        )

        for (nm in names(default_data)) {
          r$scenario_data[[nm]] <- default_data[[nm]]
        }

        # put holding message on display with instructions for what to do
        r$performance_plot <- plot_hold_message()
    })

    observeEvent(
      input$ics_selection, {
        r$trust_icb_pie_chart <- plot_trust_icb_proportions(
          ics_code = ics_code_lkp(
            input$ics_selection
          )
        )
      }
    )

    # pass plot to output
    output$performance_plot <- renderPlot({
      r$performance_plot
    }, res = 96)

    output$trust_icb_plot <- renderPlot({
      r$trust_icb_pie_chart
    })

    # scenario data tab, editing and modelling predictions ---------------------

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
          disable = list(
            columns = seq_len(
              match(first_year(),
                    names(r$scenario_data$custom_display)
                  ) - 2
              )
            ), # disable editing metric and domain fields and previous year values
          numeric = "all" # allow only numeric values
        ),
        selection = "none", # don't need to be able to select rows
        colnames = c(
          "Metric" = "metric",
          "Domain" = "domain"
        ),
        options = list(
          pageLength = 25,
          autoWidth = TRUE,
          columnDefs = list(
            list(
              targets = 0, # 1st column (0-indexed)
              createdCell = DT::JS(
                "function(td, cellData, rowData, row, col) {",
                "$(td).attr('title', cellData);",
                "}"
                # "function(td, cellData, rowData, row, col) {",
                # # "let words = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen', 'twenty'];",
                # # "let number = parseInt(cellData);",
                # "$(td).attr('title', cellData);",
                # "}"
              )
            )
          )
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

    # model the current scenario and add it to the charts
    observeEvent(
      input$btn_add_scenario_prediction, {

        update_predictions_and_plot_r(
          prediction_custom_scenario = input$custom_name,
          model_outputs = model_outputs |>
            lapply(
              function(x) x[["wf"]]
            ),
          scenario_name = "custom",
          performance_metrics = input$performance_metric_selection,
          r = r
        )

        output$performance_plot <- renderPlot({
          r$performance_plot
        }, res = 96)
      })

    # remove current scenario from chart
    observeEvent(
      input$btn_remove_scenario_prediction, {

        if (!is.null(r$predictions)) {
          r$predictions <- r$predictions |>
            filter(
              !!sym("value_type") != paste0(
                "Prediction - ",
                input$custom_name
              )
            )

          if (any(grepl("Prediction", r$predictions$value_type))) {
            r$performance_plot <- plot_performance(
              historic_data = bind_rows(
                r$ics_data,
                r$predictions
              ),
              performance_metric = input$performance_metric_selection
            )
          } else {
            r$predictions <- NULL
            r$performance_plot <- plot_hold_message()
          }
        } else {
          r$performance_plot <- plot_hold_message()
        }

        output$performance_plot <- renderPlot({
          r$performance_plot
        }, res = 96)
      })


# Exporting and importing data to the custom scenario table ---------------


    # Download a csv of the custom scenario data table
    output$download_custom_scenario_inputs <- downloadHandler(
      filename = "custom_input_data.csv",
      content = function(file) {
        write.csv(
          r$scenario_data$custom,
          file,
          row.names = FALSE
        )
      }
    )

    # loads custom file into the database to override the r$scenario_data$custom dataset
    observeEvent(
      input$custom_scenario_file, {

        if (is.null(input$custom_scenario_file)) {
          return(r$scenario_data$custom)
        }

        # read in new file
        file_custom_data <- read.csv(
          input$custom_scenario_file$datapath,
          check.names = FALSE,
          header = TRUE
        ) |>
          dplyr::as_tibble()

        # perform file checks
        pass_checks <- check_custom_inputs(
          r$scenario_data$custom,
          file_custom_data
        )

        if (pass_checks != "pass") {
          # message if checks don't pass
          showModal(
            modalDialog(
              title = "Error",
              pass_checks,
              easyClose = TRUE,
              footer = NULL
            )
          )
        } else {
          # load custom file into database if they pass
          r$scenario_data$custom <- file_custom_data
        }
      })

# Template scenarios tab; adding and removing from chart ------------------

    # Update charts depending on whether the "display linear" check box is
    # selected or not
    observeEvent(
      input$display_linear, {
        if (input$display_linear) {
          update_predictions_and_plot_r(
            prediction_custom_scenario = input$custom_name,
            model_outputs = model_outputs |>
              lapply(
                function(x) x[["wf"]]
              ),
            scenario_name = "linear",
            performance_metrics = input$performance_metric_selection,
            r = r
          )
        } else if (input$display_linear == FALSE) {
          if (!is.null(r$predictions)) {
            r$predictions <- r$predictions |>
              filter(
                !!sym("value_type") != "Prediction - linear extrapolation"
              )

            if (any(grepl("Prediction", r$predictions$value_type))) {
              r$performance_plot <- plot_performance(
                historic_data = bind_rows(
                  r$ics_data,
                  r$predictions
                ),
                performance_metric = input$performance_metric_selection
              )
            } else {
              r$predictions <- NULL
              r$performance_plot <- plot_hold_message()
            }
          } else {
            r$performance_plot <- plot_hold_message()
          }
        }


        output$performance_plot <- renderPlot({
          r$performance_plot
        }, res = 96)
      }
    )
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

        shiny::updateCheckboxInput(
          inputId = "display_linear",
          value = FALSE
        )
        shiny::updateCheckboxInput(
          inputId = "display_linear",
          value = TRUE
        )
      }
    )

    # Update charts depending on whether the "display percent" check box is
    # selected or not
    observeEvent(
      input$display_percent, {
        if (input$display_percent) {
          update_predictions_and_plot_r(
            prediction_custom_scenario = input$custom_name,
            model_outputs = model_outputs |>
              lapply(
                function(x) x[["wf"]]
              ),
            scenario_name = "percent",
            performance_metrics = input$performance_metric_selection,
            r = r
          )
        } else if (input$display_percent == FALSE) {
          if (!is.null(r$predictions)) {
            # browser()
            r$predictions <- r$predictions |>
              filter(
                !!sym("value_type") != "Prediction - percent change"
              )

            if (any(grepl("Prediction", r$predictions$value_type))) {
              r$performance_plot <- plot_performance(
                historic_data = bind_rows(
                  r$ics_data,
                  r$predictions
                ),
                performance_metric = input$performance_metric_selection
              )
            } else {
              r$predictions <- NULL
              r$performance_plot <- plot_hold_message()
            }
          } else {
            r$performance_plot <- plot_hold_message()
          }
        }

        output$performance_plot <- renderPlot({
          r$performance_plot
        }, res = 96)
      }
    )
    # update the performance chart when changes to the percent scenario is
    # applied
    observeEvent(
      input$apply_percent_change_button, {

        percent_change <- scenario_inputs(
          ics_code = r$ics_cd,
          horizon = input$horizon_selector,
          scenario = "percent",
          percent = input$percent_change_val
        )

        r$scenario_data$percent <- percent_change

        shiny::updateCheckboxInput(
          inputId = "display_percent",
          value = FALSE
        )
        shiny::updateCheckboxInput(
          inputId = "display_percent",
          value = TRUE
        )
      }
    )

    # Update charts depending on whether the "display last_known" check box is
    # selected or not
    observeEvent(
      input$display_last_known, {
        if (input$display_last_known) {
          update_predictions_and_plot_r(
            prediction_custom_scenario = input$custom_name,
            model_outputs = model_outputs |>
              lapply(
                function(x) x[["wf"]]
              ),
            scenario_name = "last_known",
            performance_metrics = input$performance_metric_selection,
            r = r
          )
        } else if (input$display_percent == FALSE) {
          if (!is.null(r$predictions)) {
            r$predictions <- r$predictions |>
              filter(
                !!sym("value_type") != "Prediction - last known value"
              )

            if (any(grepl("Prediction", r$predictions$value_type))) {
              r$performance_plot <- plot_performance(
                historic_data = bind_rows(
                  r$ics_data,
                  r$predictions
                ),
                performance_metric = input$performance_metric_selection
              )
            } else {
              r$predictions <- NULL
              r$performance_plot <- plot_hold_message()
            }
          } else {
            r$performance_plot <- plot_hold_message()
          }
        }

        output$performance_plot <- renderPlot({
          r$performance_plot
        }, res = 96)
      }
    )



# Reporting from the chart ------------------------------------------------

    output$report_btn <- downloadHandler(
      filename <-  "myreport.docx",
      content = function(file) {
        tempReport <- file.path(tempdir(), "skeleton.Rmd")

        file.copy(
          "inst/rmarkdown/templates/scenario-report/skeleton/skeleton.Rmd",
          tempReport,
          overwrite = TRUE
        )
        params <- list(
          performance_plot = r$performance_plot,
          prediction_data = r$predictions,
          scenario_data = r$scenario_data,
          ics_name = input$ics_selection
        )
        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )

      }
    )
  })


}

