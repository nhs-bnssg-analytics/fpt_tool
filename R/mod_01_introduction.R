#' 01_introduction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_introduction_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
  fluidPage(
    titlePanel("Introduction"),
    fluidRow(
      markdown("Welcome to the Demand and Capacity strategic planner tool. Please see [here](https://github.com/nhs-bnssg-analytics/d_and_c) for more details about this project.")
    )
  )
}

#' 01_introduction Server Functions
#'
#' @noRd
mod_01_introduction_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
