
<!-- README.md is generated from README.Rmd. Please edit that file -->

# planner: A Shiny Application for ICS Planning

<!-- badges: start -->

[![R-CMD-check](https://github.com/nhs-bnssg-analytics/shiny_planner/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nhs-bnssg-analytics/shiny_planner/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/nhs-bnssg-analytics/shiny_planner/branch/main/graph/badge.svg)](https://app.codecov.io/gh/nhs-bnssg-analytics/shiny_planner?branch=main)
<!-- badges: end -->

The planner package is a Shiny application designed to assist staff in
planning roles within Integrated Care Systems (ICSs) in understanding
the associations between demand and capacity metrics on NHS performance.
It leverages multiple models developed on public data to provide
insights into future NHS performance based on various demand and
capacity scenarios.

## Installation

You can install the development version of planner from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nhs-bnssg-analytics/shiny_planner")
```

## Usage

To launch the Shiny app following installation:

``` r
library(planner)
app()
```

## Functionality

- **Interactive exploration:** Users can manipulate demand and capacity
  metrics to observe their impact on future NHS performance.
- **Scenario building:** The tool enables users to create different
  future scenarios based on varying demand and capacity assumptions.
- **Model-driven insights:** Underpinned by multiple models built on
  public data, the application offers data-driven predictions.

## Contributing

Contributions to the package are welcome. Please follow the standard
GitHub workflow for submitting pull requests.

## Code of Conduct

Please note that the planner project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Additional Notes

More information about the data and modelling can be found on the
[project documentation
page](https://nhs-bnssg-analytics.github.io/d_and_c/outputs/01_index.html).
