## code to prepare `ics_names` dataset goes here
ics_names <- lookup_ics_names(with_region = TRUE)

## code to prepare `trust_ics_proportions` dataset goes here
trust_ics_props <- snapshot_trust_ics_proportions()

## metadata for the metrics
metadata <- snapshot_metadata()

## historic ics data for all of the metrics
ics_timeseries <- snapshot_ics_data()

## model accuracy
model_accuracy <- snapshot_model_accuracy()

## import model
model <- snapshot_model()

## ics lookup table
lookup_ics_table <- snapshot_ics_lookup()

usethis::use_data(
  ics_names,
  trust_ics_props,
  ics_timeseries,
  metadata,
  model_accuracy,
  model,
  lookup_ics_table,
  internal = TRUE,
  overwrite = TRUE
)
