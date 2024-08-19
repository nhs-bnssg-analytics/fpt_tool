## ics lookup table
lookup_ics_table <- snapshot_ics_lookup()

## code to prepare `ics_names` dataset goes here
ics_names <- lookup_ics_names(with_region = TRUE)

## code to prepare `trust_ics_proportions` dataset goes here
trust_ics_props <- snapshot_trust_ics_proportions()

## metadata for the metrics
metadata <- snapshot_metadata()

## replace metadata in sysdata.rda
### create new environment
new_env <- new.env(hash = FALSE)

### load current internal data into this new environment
load("R/sysdata.rda", envir = new_env)

### add or replace some objects
new_env$metadata <- metadata

### save the environment as internal package data
save(list = names(new_env),
     file = "R/sysdata.rda",
     envir = new_env)

## historic ics data for all of the metrics
ics_timeseries <- snapshot_ics_data()

## historic ics data for all of the metrics
ics_timeseries <- snapshot_ics_data()

## model accuracy
model_accuracy <- snapshot_model_accuracy()

## import model
model <- snapshot_model()

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
