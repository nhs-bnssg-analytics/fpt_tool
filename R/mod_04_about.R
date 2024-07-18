#' 04_about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_04_about_ui <- function(id){
  ns <- NS(id)

  background_card <- card(
    card_header(
      "Background",
      class = "default-card-header"
    ),
    card_body(
      p(
        HTML(
          paste(
            "Understanding system demand and capacity is essential for operational and strategic healthcare planning.",
            "A common approach to demand and capacity planning is spreadsheet modelling and simulating patient flow through the health system.",
            "Within each Integrated Care System (ICS) these kinds of models are possible to make, and often already exist.",
            "It is much harder to do this at a regional level because the structures and populations in each ICS are very different to one another.",
            "A tool that is developed to try and capture the intricacies and detail in one ICS is therefore likely to be unrepresentative and open to challenge if used to inform local decision making in another.",
            "Additionally, each ICS has developed their own bespoke tools to understand demand and capacity, which already supports local decision making, particularly in the acute setting.",
            "<br><br>This tool has been developed using only publicly available data for the whole of England.",
            "These data are described in more detail on the ",
            "<a href = 'https://nhs-bnssg-analytics.github.io/d_and_c/outputs/Data/data_metadata.html' target='_blank'>modelling inputs</a> page of the report documentation.",
            "The methods underpinning data processing and modelling can be found <a href='https://nhs-bnssg-analytics.github.io/d_and_c/outputs/01_index.html' target='_blank'>here</a>.",
            "The analysis and modelling was done using R and has been made publicly available <a href='https://github.com/nhs-bnssg-analytics/d_and_c' target='_blank'>here</a>.",
            "<br><br>This tool should be used to support and build cases for medium term investment.",
            "It can be used in different ways.",
            "For example, with a particular performance metric in mind, it can be used to see what mix of investment over time can help improve performance.",
            "Separately, it can be used to understand how investment in a particular capacity input, or population health management programme which will result in changes to demand, will affect multiple performance metrics."
          )
        )
      )
    )
  )

  limitations_card <- card(
    card_header(
      "Limitations",
      class = "default-card-header"
    ),
    card_body(
      p(
        HTML(
          paste(
            "A number of data limitations need to be considered:",
            "<ol><li>Missing data - the analysis used publicly available data, so there were useful facets of information that are excluded from this process because data were not published.",
            "This can result in unintuitive relationships in the models that are built.",
            "Additionally, useful metrics were excluded where they had not been published for a long enough period or they had changed definition significantly.</li>",
            "<li>Data quality - metrics were compiled for this research entirely using programming methods.",
            "While the best efforts were made to identify issues with the collated data, it is likely that local issues to published data may not have been identified in the collation exercise, e.g., where a publication provides a warning about a specific data item for a specific period in that published dataset.</li>",
            "<li>Geographies - data are published at different geographies, for example Local Authority geography, Acute Trust geography or ICS geography.",
            "While some geographies map coterminously with ICS geographies, some approximate methods of aggregation were needed for the ones that did not.</li>",
            "<li>Time periods of published data - data were published at different time periods, generally based on regulatory requirements.",
            "Therefore, the latest information was available for some metrics but not others.</li>",
            "<li>Frequencies of published data - the frequency of publication varied between metrics.",
            "Metrics were published by month, quarter, or year (both financial or calendar).",
            "Monthly and quarterly counts were aggregated to calendar year by summing the higher frequency data.",
            "Where the data were already a calculation at the higher frequency, a mean was calculated in the aggregation process.</li>",
            "<li>Comparable metrics - Data were generally published as raw figures.",
            "To provide context to the data, the metric was combined with a denominator to calculate a standardised value.",
            "The choice of denominator had a large influence on how the metric differed between ICSs.</li>",
            "<li>COVID-19 pandemic - the pandemic period has a large influence on many metrics.",
            "Each metric may have been influenced in different ways and not represent business-as-usual, therefore relationships between metrics may drastically differ during that period from the non-pandemic period.",
            "This will impact any predictive model.</li></ol>",
            "There were also some limitations around modelling.",
            "The models used in this tool are the best performing models from a series of approaches evaluated.",
            "<ol><li>Random forest predicts outside of bounds - some models use a random forest approach, where the model predicts the change in proportion from the previous year.",
            "This means that the predicted change is bound by previously seen changes.",
            "It is therefore possible that values otuside of the 0 to 100 window, which are the bounds of the performance metrics as they are all proportions, can be predicted.</li>",
            "<li>Generalised linear model (GLM) reaching bounds - some models use the performance metric from the previous year as one of the predictor variables.",
            "For the GLM model where this is the case, it is possible that each year's performance metric can increase (or decrease), causing the following year's prediction to increase (or decrease).",
            "This can ultimately lead to the prediction reaching the bounds of the performance metric.</li></ol>"
          )
        )
      )
    )
  )

  contact_us_card <- card(
    card_header(
      "Contact us",
      class = "default-card-header"
    ),
    card_body(
      p(
        HTML(
          paste(
            "If you have any feedback or questions, please get in touch at",
            "<a href='mailto:sebastian.fox3@nhs.net?subject=Demand Capacity planning tool'>sebastian.fox3@nhs.net</a>"
          )
        )
      )
    )
  )

  tagList(
    bslib::page_fluid(
      background_card,
      limitations_card,
      contact_us_card
    )
  )
}

#' 04_about Server Functions
#'
#' @noRd
mod_04_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_04_about_ui("04_about_1")

## To be copied in the server
# mod_04_about_server("04_about_1")
