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
            "<br><br>This tool has been developed using only publicly available data for the whole of England (see references, below, for more detail).",
            "<br><br>This tool should be used to support and build cases for medium term investment.",
            "It can be used in different ways.",
            "For example, with a particular performance metric in mind, it can be used to see what mix of interventions over time can help improve performance.",
            "Separately, it can be used to understand how interventions towards a particular capacity input, or population health management programme which will result in changes to demand, will affect multiple performance metrics."
          )
        )
      )
    )
  )

  references_card <- card(
    card_header(
      "References",
      class = "default-card-header"
    ),
    card_body(
      p(
        HTML(
          paste(
            "The underpinning data are described in more detail on the ",
            "<a href = 'https://nhs-bnssg-analytics.github.io/fpt_analysis/outputs/Data/data_metadata.html' target='_blank'>modelling inputs</a> page of the report documentation,",
            "and a csv file of the data can be downloaded <a href = 'https://github.com/nhs-bnssg-analytics/fpt_analysis/blob/master/ad_hoc/full_data.csv' target='_blank'>here</a>.",
            "The methods underpinning data processing and modelling can be found <a href='https://nhs-bnssg-analytics.github.io/fpt_analysis/outputs/01_index.html' target='_blank'>here</a>.",
            "The analysis and modelling was done using R and the reproducible code has been made publicly available <a href='https://github.com/nhs-bnssg-analytics/fpt_analysis' target='_blank'>here</a>."
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
            "<ol><li><b>Missing data</b> - the analysis used publicly available data, so useful facets of information were excluded from this process because data were not available.",
            "This can result in unintuitive relationships in the models that are built.",
            "Additionally, useful metrics were excluded where they had not been published for a long enough period or they had changed definition significantly.</li>",
            "<li><b>Data quality</b> - metrics were compiled for this research entirely using programming methods.",
            "While the best efforts were made to identify issues with the collated data, it is likely that local issues to published data may not have been identified in the collation exercise, e.g., where a publication provides a warning about a specific data item for a specific period in that published dataset.</li>",
            "<li><b>Geographies</b> - data are published at different geographies, for example Local Authority geography, Acute Trust geography or ICS geography.",
            "While some geographies map coterminously with ICS geographies, some approximate methods of aggregation were needed for the ones that did not.</li>",
            "<li><b>Time periods of published data</b> - data were published at different time periods, generally based on regulatory requirements.",
            "Therefore, the latest information was available for some metrics but not others.</li>",
            "<li><b>Frequencies of published data</b> - the frequency of publication varied between metrics.",
            "Metrics were published by month, quarter, or year (both financial or calendar).",
            "Monthly and quarterly counts were aggregated to calendar year by summing the higher frequency data.",
            "Where the data were already a calculation at the higher frequency, a mean was calculated in the aggregation process.</li>",
            "<li><b>Comparable metrics</b> - Data were generally published as raw figures.",
            "To provide context to the data, the metric was combined with a denominator to calculate a standardised value.",
            "The choice of denominator had a large influence on how the metric differed between ICSs.</li>",
            "<li><b>COVID-19 pandemic</b> - the pandemic period has a large influence on many metrics.",
            "Each metric may have been influenced in different ways and not represent business-as-usual, therefore relationships between metrics may drastically differ during that period from the non-pandemic period.",
            "This will impact any predictive model.</li></ol>"
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
            "<a href='mailto:sebastian.fox3@nhs.net?subject=Future performance tool'>sebastian.fox3@nhs.net</a>",
            "(Senior Data Scientist in the South West Decision Support Network)."
          )
        )
      )
    )
  )

  tagList(
    bslib::page_fluid(
      background_card,
      limitations_card,
      references_card,
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


