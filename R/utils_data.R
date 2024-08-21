# check custom data inputs ------------------------------------------------

check_custom_inputs <- function(database_table, custom_table) {

  check_names <- identical(
    names(database_table),
    names(custom_table)
  )

  check_metrics <- identical(
    database_table |> select(1:2),
    custom_table |> select(1:2)
  )

  if (!check_names) {
    check_response <- "Field names in file are not what are expected"
  } else if (!check_metrics) {
    check_response <- "First two columns of file are not what are expected"
  } else {
    check_response <- "pass"
  }

  return(check_response)
}
