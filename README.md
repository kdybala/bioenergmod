<!-- README.md is generated from README.Rmd. Please edit that file -->
bioenergmod
===========

The bioenergmod package contains a set of functions developed for bioenergetics modeling, to compare the daily energy requirements of a wildlife population of interest to the daily energy supply available, taking into account dynamic habitat availability and consumption in previous time steps. This was primarily developed with wetland-dependent shorebirds in mind, to account for variation in the proportion of potential habitat that is flooded (i.e. has open water) and variation in the proportion of the open water that is accessible (i.e. shallow enough for use by foraging shorebirds).

Features
--------

-   Accommodates multiple land cover types with differing availability and value
-   Accommodates tracking energy supply in all habitat vs. accessible habitat
-   Includes option for Monte Carlo simulation

Installation
------------

You can install this package directly from R using the **devtools** package:

``` r
devtools::install_github("kdybala/bioenergmod")
```