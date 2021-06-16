
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![rcc](https://github.com/krlmlr/mlfit/workflows/rcc/badge.svg)](https://github.com/krlmlr/mlfit/actions)
[![codecov.io](https://codecov.io/github/krlmlr/mlfit/coverage.svg?branch=master)](https://codecov.io/github/krlmlr/mlfit?branch=master)
[![Codecov test
coverage](https://codecov.io/gh/krlmlr/mlfit/branch/master/graph/badge.svg)](https://codecov.io/gh/krlmlr/mlfit?branch=master)
<!-- badges: end -->

Implementation of algorithms that extend Iterative Proportional Fitting
(IPF) to nested structures.

The IPF algorithm operates on count data. This package offers
implementations for several algorithms that extend this to nested
structures: “parent” and “child” items for both of which constraints can
be provided.

## Example

Here is a multi-level fitting example with a reference sample and two
control tables. The reference sample represents a sample of a
population. The first control table, `individual_control`, is for
individual level and the second table, `group_control`, is for group
level. Each row of the reference sample represents an individual, where
`HHNR` is their group ID and `PNR` is their individual ID, `APER` and
`WKSTAT` are individial-level charateristics, and `CAR` is the only
household characteristic of the sample population. The ‘N’ columns in
both control tables denote how many units of individuals or groups
belong to each category.

``` r
library(mlfit)
library(tibble)

reference_sample <- tibble::tribble(
  ~HHNR, ~PNR, ~APER, ~CAR, ~WKSTAT,
     1L,   1L,    3L,  "0",     "1",
     1L,   2L,    3L,  "0",     "2",
     1L,   3L,    3L,  "0",     "3",
     2L,   4L,    2L,  "0",     "1",
     2L,   5L,    2L,  "0",     "3",
     3L,   6L,    3L,  "0",     "1",
     3L,   7L,    3L,  "0",     "1",
     3L,   8L,    3L,  "0",     "2",
     4L,   9L,    3L,  "1",     "1",
     4L,  10L,    3L,  "1",     "3",
     4L,  11L,    3L,  "1",     "3",
     5L,  12L,    3L,  "1",     "2",
     5L,  13L,    3L,  "1",     "2",
     5L,  14L,    3L,  "1",     "3",
     6L,  15L,    2L,  "1",     "1",
     6L,  16L,    2L,  "1",     "2",
     7L,  17L,    5L,  "1",     "1",
     7L,  18L,    5L,  "1",     "1",
     7L,  19L,    5L,  "1",     "2",
     7L,  20L,    5L,  "1",     "3",
     7L,  21L,    5L,  "1",     "3",
     8L,  22L,    2L,  "1",     "1",
     8L,  23L,    2L,  "1",     "2"
  )

individual_control <- tibble::tribble(
  ~WKSTAT,   ~N,
      "1",  91L,
      "2",  65L,
      "3", 104L
  )

group_control <- tibble::tribble(
  ~CAR,  ~N,
   "0", 35L,
   "1", 65L
  )
```

First we need to create a `ml_problem` object which defines our
multi-level fitting problem. `special_field_names()` is useful for the
`field_names` argument to `ml_problem()`, this is where we need to
specific the names of the ID columns in our reference sample and the
count column in the control tables.

``` r
fitting_problem <- ml_problem(
  ref_sample = reference_sample, 
  controls = list(
    individual = list(individual_control),
    group = list(group_control)
  ),
  field_names = special_field_names(
    groupId = "HHNR", 
    individualId = "PNR", 
    count = "N"
  )
)
```

You can use one of the `ml_fit_*()` functions to calibrate your fitting
problem, or you can use
`ml_fit(ml_problem, algorithm = "<your-selected-algorithm>")`.

``` r
fit <- ml_fit(ml_problem = fitting_problem, algorithm = "ipu")
fit
#> An object of class ml_fit
#>   Algorithm: ipu
#>   Success: TRUE
#>   Residuals (absolute): min = -6.41906e-05, max = 0
#>   Flat problem:
#>   An object of class flat_ml_fit_problem
#>     Dimensions: 5 groups, 8 target values
#>     Model matrix type: separate
#>     Original fitting problem:
#>     An object of class ml_problem
#>       Reference sample: 23 observations
#>       Control totals: 1 at individual, and 1 at group level
```

`mlfit` also provides a function that helps to replicate the reference
sample based on the fitted/calibrated weights. See `?ml_replicate` to
find out which integerisation algorithms are available.

``` r
syn_pop <- ml_replicate(fit, algorithm = "trs")
syn_pop
#> # A tibble: 262 x 5
#>     HHNR   PNR  APER CAR   WKSTAT
#>    <int> <int> <int> <chr> <chr> 
#>  1     1     1     3 0     1     
#>  2     1     2     3 0     2     
#>  3     1     3     3 0     3     
#>  4     2     4     2 0     1     
#>  5     2     5     2 0     3     
#>  6     3     6     2 0     1     
#>  7     3     7     2 0     3     
#>  8     4     8     2 0     1     
#>  9     4     9     2 0     3     
#> 10     5    10     2 0     1     
#> # … with 252 more rows
```

## Powered by

-   [`grake`](http://krlmlr.github.io/grake): A reimplementation of
    generalized raking ([Deville and Särndal,
    1992](http://amstat.tandfonline.com/doi/abs/10.1080/01621459.1992.10475217);
    [Deville, Särndal and Sautory,
    1993](http://www.tandfonline.com/doi/abs/10.1080/01621459.1993.10476369))

## Related work

-   [`wrswoR`](http://krlmlr.github.io/wrswoR): An implementation of
    fast weighted random sampling without replacement ([Efraimidis and
    Spirakis,
    2006](http://www.sciencedirect.com/science/article/pii/S002001900500298X))
-   [`mangow`](http://krlmlr.github.io/mangow): Embed the Gower distance
    metric in L1
-   [`RANN.L1`](https://github.com/jefferis/RANN/tree/master-L1#readme):
    k-nearest neighbors using the L1 metric

## Installation

The package is not currently on CRAN, but can be installed from GitHub
with:

``` r
# install.packages("devtools")
devtools::install_github("krlmlr/mlfit")
```

### Where is `MultiLeveLIPF`?

From version `0.4.0` onwards the package is now to be known as `mlfit`.
If you would like to install any version that is older than `0.4.0`
please use:

``` r
# See https://github.com/krlmlr/mlfit/releases for the releases that are available
# To install a certain branch or commit or tag, append it to the repo name, after an @:
devtools::install_github("krlmlr/MultiLevelIPF@v0.3-7")
```
