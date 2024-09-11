# planner 0.1.3

* fixes bug when loading csv in as custom table, when csv contains commas for big numbers
* allows csv table to have metrics in a different order to what appears on the page
* deselects template checkboxes when new ICS or metrics group are selected
* updating the horizon selector while a template scenario is selected will extend/shorten that template scenario appropriately

# planner 0.1.2 (2024-08-20)

* Fixed issue where the report template couldn't be located when the package was installed.

# planner 0.1.1 (2024-08-14)

* Fixed bug for models that were modelling the change in value from the previous year.
* Simplified some of the variables, so there are fewer inputs

  * Day and night beds have been combined for the bed occupancy metric as well as the beds per 60+ population
  * Clinical workforce FTEs have been combined into a "Total" metric rather than having the individual staff groups
