# `owidR`

This package acts as an interface to the [Our World in Data](https://ourworldindata.org/) datasets [GitHub repository](https://github.com/owid/owid-datasets), allowing for easy way to search from a list of almost 1,000 datasets and load them into the R environment.

## Installation

```
devtools::install_github("piersyork/owidR")
```

## Using the package
There are three core functions in the `owidR` package. `owid_get_datasets()` returns a tibble of all available Our World in Data datasets alongside a generated id. `owid_search()` makes it easy to search through the available datasets. `owid()` takes an id and returns the corresponding dataset.

# Example
Get Covid-19 policy stringency data.
``` r
library(owidR)
library(dplyr)
ds <- owid_get_datasets()

id <- owid_search(ds, "COVID Government Response ") %>% pull(id)

covid_policy <- owid(ds, id)

covid_policy
# # A tibble: 82,442 x 23
#    Entity       Year school_closures workplace_closures cancel_public_events
#  * <chr>       <dbl>           <dbl>              <dbl>                <dbl>
#  1 Afghanistan     0               0                  0                    0
#  2 Afghanistan     1               0                  0                    0
#  3 Afghanistan     2               0                  0                    0
#  4 Afghanistan     3               0                  0                    0
#  5 Afghanistan     4               0                  0                    0
#  6 Afghanistan     5               0                  0                    0
#  7 Afghanistan     6               0                  0                    0
#  8 Afghanistan     7               0                  0                    0
#  9 Afghanistan     8               0                  0                    0
# 10 Afghanistan     9               0                  0                    0
# # … with 82,432 more rows, and 18 more variables:
# #   close_public_transport <dbl>, public_information_campaigns <dbl>,
# #   restrictions_internal_movements <dbl>,
# #   international_travel_controls <dbl>, fiscal_measures <dbl>,
# #   emergency_investment_healthcare <dbl>, investment_vaccines <dbl>,
# #   contact_tracing <dbl>, stringency_index <dbl>,
# #   restriction_gatherings <dbl>, stay_home_requirements <dbl>,
# #   income_support <dbl>, debt_relief <dbl>, international_support <dbl>,
# #   testing_policy <dbl>, containment_index <dbl>, facial_coverings <dbl>,
# #   vaccination_policy <dbl>
```

