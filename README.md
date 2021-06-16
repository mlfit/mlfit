
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
