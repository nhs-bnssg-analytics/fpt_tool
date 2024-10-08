#' 03_model_info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList plotOutput
#' @importFrom bslib card card_header card_body
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_03_model_info_ui <- function(id){
  ns <- NS(id)
  accuracy_card <- card(
    fill = FALSE,
    card_header(
      "Are the predictions reliable?",
      class = "default-card-header"
    ),
    card_body(
      p(
        HTML(
          paste(
            "The predictions for each performance metricare made with a different model.",
            "The Mean Absolute Percentage Error for each model is presented in the chart below.",
            "The values in the chart below refer to the average error (the difference between the observed and the predicted values) as a percentage of the observed value for the data that the models were tested on.",
            "A number of 10% in the charts below can be interpreted as 'on average, the prediction error is 10% of the real value for this model'."
          )
        )
      )
    ),
    plotOutput(
      ns("model_accuracy_plot"),
      width = "700px"
    )
  )

  variable_importance_card <- card(
    card_header(
      "Which are the important variables?",
      class = "default-card-header"
    ),
    card_body(
      p(
        paste(
          "The important variables that influence each model are different.",
          "The models are built by throwing in all of input variables, which are not necessarily related to the outcome. The modelling process will identify the variables most related to the outcome.",
          "These variabels have a higher influence on the outcome.",
          "Bare in mind that the most related variables identified will not necessarily have a causal impact on the outcome. This process cannot determine that."
          )
        ),
      p(
        HTML(
          paste(
          "Use the following charts <i>along with other evidence</i> to justify strategies.<br><br>",
          "The important variables from the models are calculated using a method called 'permutation importance'.",
          "This method takes the existing model, then calculates the increase in the error when the values for each input metric in turn is shuffled at random. This shuffling process is done 10 times for each input metric, with an average and standard deviation calculated."
          )
        )
      )
    ),
    plotOutput(
      ns("permutation_importance_plot"),
      width = "900px",
      height = "800px"
    )
  )

  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = "1000px",
      fixed_width = TRUE,
      heights_equal = "row",
      accuracy_card,
      variable_importance_card
    )
  )
}

#' 03_model_info Server Functions
#'
#' @import ggplot2
#' @importFrom tidytext scale_y_reordered reorder_within
#' @importFrom stringr str_wrap
#' @importFrom rlang sym
#' @importFrom purrr list_rbind
#' @importFrom dplyr mutate group_by top_n
#' @noRd
mod_03_model_info_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # model_accuracy is an internal dataset to this package
    output$model_accuracy_plot <- renderPlot({
      model_accuracy |>
        ggplot(
          aes(
            x = !!sym("mape"),
            y = stringr::str_wrap(!!sym("target_variable"), 40)
          )
        ) +
        geom_col(
          colour = "black",
          fill = "lightgrey"
        ) +
        theme_bw() +
        labs(
          y = "",
          x = "Mean absolute percentage error (%)"
        ) +
        scale_y_discrete(
          limits = rev
        )
    }, res = 96)

    output$permutation_importance_plot <- renderPlot({
      load_model_object(type = "perm_imp") |>
        purrr::list_rbind(
          names_to = "target_variable"
        ) |>
        mutate(
          Variable = stringr::str_wrap(!!sym("Variable"), 40),
          target_variable = stringr::str_wrap(!!sym("target_variable"), 30)
        ) |>
        group_by(!!sym("target_variable")) |>
        dplyr::top_n(
          n = 5,
          wt = !!sym("Importance")
        ) |>
        ggplot(
          aes(
            x = !!sym("Importance"),
            y = tidytext::reorder_within(
              !!sym("Variable"), !!sym("Importance"), !!sym("target_variable")
            )
          )
        ) +
        geom_col(
          colour = "black",
          fill = "lightgrey"
        ) +
        geom_errorbarh(
          aes(
            xmin = !!sym("Importance") - !!sym("StDev"),
            xmax = !!sym("Importance") + !!sym("StDev")
          ),
          height = 0.2
        ) +
        tidytext::scale_y_reordered() +
        facet_wrap(
          facets = vars(!!sym("target_variable")),
          scales = "free_y",
          ncol = 2
        ) +
        theme_bw() +
        labs(
          y = "",
          x = "Average increase in the mean absolute percentage error"
        )
    }, res = 96)
  })
}

