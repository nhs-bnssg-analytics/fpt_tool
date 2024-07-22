#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  r <- reactiveValues()
  mod_01_introduction_server("01_introduction_1")
  mod_02_scenario_planner_server("02_scenario_planner_1", r = r)
  mod_03_model_info_server("03_model_info_1")
  mod_04_about_server("04_about_1")
}
