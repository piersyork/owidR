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
```

