#' vector of ICS names
#'
#' @description provides character vector of ICS names in alphabetical order
#'
#' @return character vector
#' @importFrom dplyr pull
#' @importFrom rlang sym
#' @importFrom utils stack
#' @noRd
lookup_ics_names <- function(with_regions = FALSE) {
  ics_nms <- lookup_ics_table

  if (with_regions == TRUE) {
    ics_nms <- ics_nms |>
      select("NHSER22NM", "ICB22NM") |>
      arrange(
        !!sym("NHSER22NM"),
        !!sym("ICB22NM")
      ) |>
      rev() |>
      utils::unstack() |>
      as.list()
  } else {
    ics_nms <- ics_nms |>
      dplyr::pull(!!sym("ICB22NM")) |>
      sort()
  }


  return(ics_nms)
}


#' ICS22 health code from name
#'
#' @param ics_name string; ICB name beginning with "NHS"
#'
#' @return single item character vector beginning with "Q" and three letters
#'   long
#'
#' @importFrom dplyr pull filter
#' @importFrom rlang sym
#' @noRd
ics_code_lkp <- function(ics_name) {
  ics_code <- lookup_ics_table |>
    filter(!!sym("ICB22NM") == ics_name) |>
    pull(!!sym("ICB22CDH"))
  return(ics_code)
}

#' ICS22 name from health code
#'
#' @param ics_code string; three letters, beginning with "Q"
#'
#' @return single item character vector
#'
#' @importFrom dplyr pull filter
#' @importFrom rlang sym
#' @noRd
ics_name_lkp <- function(ics_code) {
  ics_name <- lookup_ics_table |>
    filter(!!sym("ICB22CDH") == ics_code) |>
    pull(!!sym("ICB22NM"))
  return(ics_name)
}

#' Load model object, or part of model object
#'
#' @param type character(1); either "wf" or "perm_imp"
#'
#' @noRd
load_model_object <- function(type = NULL) {

  if (!is.null(type)) {
    type <- match.arg(
      type,
      c("wf", "perm_imp")
    )
# model is an internal data object
    model <- model |>
      lapply(
        \(x) x[[type]]
      )
  }

  return(model)
}

#' first year for forecasting
#' @return integer for first year for forecasting
#' @noRd
first_year <- function() {
  return(2024L)
}
