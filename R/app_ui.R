#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib navset_tab nav_panel nav_spacer nav_item nav_menu
#' @noRd
app_ui <- function(request) {
  github_shiny <- tags$a(
    shiny::icon("github"),
    "Shiny",
    href = "https://github.com/nhs-bnssg-analytics/fpt_tool",
    target = "_blank"
  )

  github_analysis <- tags$a(
    shiny::icon("github"),
    "Analysis",
    href = "https://github.com/nhs-bnssg-analytics/fpt_analysis",
    target = "_blank"
  )

  slides <- tags$a(
    shiny::icon("file-powerpoint"),
    "Slides",
    href = "https://github.com/nhs-bnssg-analytics/fpt_analysis/blob/master/ad_hoc/presentations/20240926%20APHA%20Spatio-temporal%20strategic%20planner.pptx",
    target = "_blank"
  )

  data_file <- tags$a(
    shiny::icon("table"),
    "Raw data",
    href = "https://github.com/nhs-bnssg-analytics/fpt_analysis/blob/master/ad_hoc/full_data.csv",
    target = "_blank"
  )

  write_up <- tags$a(
    shiny::icon("pen-nib"),
    "Write up",
    href = "https://nhs-bnssg-analytics.github.io/fpt_analysis/outputs/01_index.html",
    target = "_blank"
  )

  email <- tags$a(
    shiny::icon("envelope"),
    "Contact us",
    href = "mailto:sebastian.fox3@nhs.net?subject=Future performance tool",
    target = "_blank"
  )

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    h1(
      paste0(
        "Future Performance Tool (version ",
        packageVersion("fptool"),
        ")"
      )
    ),
    p("An ICS-level demand and capacity scenario model for medium-term performance projections."),
    tagList(
      navset_tab(
        nav_panel(
          title = "How to use the tool",
          mod_01_introduction_ui("01_introduction_1")
        ),
        nav_panel(
          title = "Scenario planner",
          mod_02_scenario_planner_ui("02_scenario_planner_1")
        ),
        nav_panel(
          title = "How good are the models?",
          mod_03_model_info_ui("03_model_info_1")
        ),
        nav_panel(
          title = "About",
          mod_04_about_ui("04_about_1")
        ),
        nav_spacer(),
        nav_menu(
          title = "Links",
          nav_item(github_shiny),
          nav_item(github_analysis),
          nav_item(slides),
          nav_item(data_file),
          nav_item(write_up),
          nav_item(email)
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
      app_title = "fptool"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
