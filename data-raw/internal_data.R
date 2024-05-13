## code to prepare `ics_names` dataset goes here
ics_names <- lookup_ics_names(with_region = TRUE)

## code to prepare `trust_ics_proportions` dataset goes here
trust_ics_props <- trust_ics_proportions()

usethis::use_data(
  ics_names, trust_ics_props,
  internal = TRUE,
  overwrite = TRUE
)
