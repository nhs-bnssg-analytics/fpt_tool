## code to prepare `ics_names` dataset goes here
ics_names <- lookup_ics_names(with_region = TRUE)

## code to prepare `trust_ics_proportions` dataset goes here
trust_ics_props <- trust_ics_proportions()

## historic ics data for all of the metrics
ics_timeseries <- snapshot_ics_data()

## metadata for the metrics
metadata <- snapshot_metadata()

## model accuracy
model_accuracy <- snapshot_model_accuracy()

## import model
model <- snapshot_model()

## ics lookup table
lookup_ics_table <- snapshot_ics_lookup()

## trust to ics proportions
trust_ics_proportions <- snapshot_trust_ics_proportions()

usethis::use_data(
  ics_names,
  trust_ics_props,
  ics_timeseries,
  metadata,
  model_accuracy,
  model,
  lookup_ics_table,
  trust_ics_proportions,
  internal = TRUE,
  overwrite = TRUE
)
