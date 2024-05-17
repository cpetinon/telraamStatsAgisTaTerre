  <!-- badges: start -->
  [![R-CMD-check](https://github.com/agistaterre/telraamStatsAgisTaTerre/actions/workflows/R-CMD-check.yaml)](https://github.com/agistaterre/telraamStatsAgisTaTerre/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# telraamStatsAgisTaTerre

The aim of this package is to grant the user tools for data visualisation and data analysis of mobility data for Telraam sensors, especially it develops functionnalities for this [`application`](https://agistaterre.shinyapps.io/mov-around/).

[`Link to the Github`](https://github.com/agistaterre/mov-around)

# Licence

[![CC BY-SA 4.0](https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg)](http://creativecommons.org/licenses/by-sa/4.0/)

This work is licensed under a [Creative Commons Attribution-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-sa/4.0/).

[![CC BY-SA 4.0](https://licensebuttons.net/l/by-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-sa/4.0/)

# Dependencies

To smoothly run this package, you will need to make the following installs :

``` r
install.packages(c("cowplot",
    "CPAT",
    "dplyr",
    "ggplot2",
    "httr",
    "jsonlite",
    "lubridate",
    "mgcv",
    "purrr",
    "readr",
    "rlang",
    "synchrony",
    "tibble",
    "zoo"))
```

If you want to install this package, you can use :

``` r
devtools::install_github("https://github.com/agistaterre/telraamStatsAgisTaTerre")
```
