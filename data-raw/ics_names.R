## code to prepare `ics_names` dataset goes here
ics_names <- lookup_ics_names(with_region = TRUE)
usethis::use_data(
  ics_names,
  internal = TRUE,
  overwrite = TRUE
)
