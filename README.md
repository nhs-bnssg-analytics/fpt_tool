
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fptool: A Shiny Application for ICS Planning

<!-- badges: start -->

[![R-CMD-check](https://github.com/nhs-bnssg-analytics/fpt_tool/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nhs-bnssg-analytics/shiny_planner/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/nhs-bnssg-analytics/fpt_tool/graph/badge.svg?token=XE87POPK37)](https://codecov.io/gh/nhs-bnssg-analytics/shiny_planner)
<!-- badges: end -->

The fptool package is a Shiny application designed to assist staff in
planning roles within Integrated Care Systems (ICSs) in understanding
the associations between demand and capacity metrics on NHS performance.
It leverages multiple models developed on public data to provide
insights into future NHS performance based on various demand and
capacity scenarios.

## Run without installing

If you have R installed on your machine, the following command may work
to run the application without installing the package:

``` r
# install.packages("shiny")
shiny::runGitHub("fpt_tool", "nhs-bnssg-analytics")
```

You may also be able to access the application on
[shinapps.io](https://sw-dsn.shinyapps.io/future-performance-tool/).

## Installation

You can install the development version of fptool from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github(
  "nhs-bnssg-analytics/fpt_tool",
  dependencies = "Suggests"
)
```

## Usage

To launch the Shiny app following installation:

``` r
fptool::run_app()
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

Please note that the fptool project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Additional Notes

More information about the data and modelling can be found on the
[project documentation
page](https://nhs-bnssg-analytics.github.io/fpt_analysis/outputs/01_index.html).
