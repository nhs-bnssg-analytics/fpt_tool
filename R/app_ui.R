#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib navset_tab nav_panel
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    h1("Demand and Capacity Strategic Planner"),
    tagList(
      navset_tab(
        nav_panel(
          title = "About",
          mod_01_introduction_ui("01_introduction_1")
        ),
        nav_panel(
          title = "Scenario planner",
          mod_02_scenario_planner_ui("02_scenario_planner_1")
        ),
        nav_panel(
          title = "How good are the models?",
          mod_03_model_info_ui("03_model_info_1")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "planner"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
