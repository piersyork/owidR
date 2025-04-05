owidR
================

<!-- badges: start -->

![CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/owidR)
[![CRAN
status](https://www.r-pkg.org/badges/version/owidR)](https://CRAN.R-project.org/package=owidR)
[![R-CMD-check](https://github.com/piersyork/owidR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/piersyork/owidR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package acts as an interface to [Our World in
Data](https://ourworldindata.org/) datasets, allowing for an easy way to
search through data used in over 3,000 charts and load them into the R
environment.

After some updates to the Our World in Data site, some of the previous
parts of the package are no longer working. The site also now provides
an API, this may offer an easier way to download datasets into R without
a dependency on this package. For example using just `data.table` you
can easily download a specific dataset. See more information on the API
here: [Our World in Data
API](https://docs.owid.io/projects/etl/api/chart-api/#chart-data-api)

``` r
library(data.table)
life_expectancy <- fread("https://ourworldindata.org/grapher/life-expectancy.csv")
```

## Installation

To install from CRAN:

``` r
install.packages("owidR")
```

To install the development version from GitHub:

``` r
devtools::install_github("piersyork/owidR")
```

## Using the package

The main function in owidR is `owid()`, which takes a chart id and
returns a data.table of the corresponding OWID dataset. To search for
chart ids you can use `owid_search()` to list all the chart ids that
match a keyword or regular expression.

## Example

Lets use the core functions to get data on how human rights have changed
over time. First by searching for charts on human rights.

``` r
library(owidR)

owid_search("human rights") # this function is no longer working
##      chart_id title
```

Let’s use the v-dem human rights index dataset.

``` r

rights <- owid("human-rights-index-vdem", rename = "vdem_rights")

rights
##             entity   code  year vdem_rights
##             <char> <char> <int>       <num>
##     1: Afghanistan    AFG  1789       0.121
##     2: Afghanistan    AFG  1790       0.121
##     3: Afghanistan    AFG  1791       0.121
##     4: Afghanistan    AFG  1792       0.121
##     5: Afghanistan    AFG  1793       0.121
##    ---                                     
## 33735:    Zimbabwe    ZWE  2020       0.416
## 33736:    Zimbabwe    ZWE  2021       0.390
## 33737:    Zimbabwe    ZWE  2022       0.390
## 33738:    Zimbabwe    ZWE  2023       0.394
## 33739:    Zimbabwe    ZWE  2024       0.374
```

ggplot2 makes it easy to visualise our data.

``` r
library(ggplot2)
library(dplyr)

rights |> 
  filter(entity %in% c("United Kingdom", "France", "United States")) |> 
  ggplot(aes(year, vdem_rights, colour = entity)) +
  geom_line()
```

<img src="inst/images/plot-1.png" style="display: block; margin: auto;" />

## COVID-19 Data

You can quickly download world covid-19 data, including vaccination
rates, using `owid_covid()`.

``` r
covid <- owid_covid()
```

## To-do

- [ ] Add function to load multiple country datasets into one dataframe
- [ ] Add caching of data (inc. backend)
- [x] Remove interactive plotting to reduce dependencies
- [ ] Create way to import owid explorers
