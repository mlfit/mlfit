
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/mlfit)](https://CRAN.R-project.org/package=mlfit)
[![rcc](https://github.com/mlfit/mlfit/workflows/rcc/badge.svg)](https://github.com/mlfit/mlfit/actions)
[![Codecov test
coverage](https://codecov.io/gh/mlfit/mlfit/branch/master/graph/badge.svg)](https://codecov.io/gh/mlfit/mlfit?branch=master)
[![](https://cranlogs.r-pkg.org/badges/mlfit)](https://cran.r-project.org/package=mlfit)
[![](https://cranlogs.r-pkg.org/badges/grand-total/mlfit)](https://CRAN.R-project.org/package=mlfit)
<!-- badges: end -->

Implementation of algorithms that extend Iterative Proportional Fitting
(IPF) to nested structures.

The IPF algorithm operates on count data. This package offers
implementations for several algorithms that extend this to nested
structures: “parent” and “child” items for both of which constraints can
be provided.

## Installation

Install from CRAN with:

``` r
install.packages("mlfit")
```

Or the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("mlfit/mlfit")
```

## `mlfit`’s Workflow

The figure below shows the `mlfit` workflow.

(to be added)

## Example - single zone

Here is a multi-level fitting example with a reference sample
(`reference_sample`) and two control tables (`individual_control` and
`group_control`). Each row of `reference_sample` represents an
individual in a sample of a population, where `HHNR` is their group ID
and `PNR` is their individual ID, `APER` and `WKSTAT` are
individial-level charateristics, and `CAR` is the only household
characteristic of the sample population. The ‘N’ columns in both control
tables denote how many units of individuals or groups belong to each
category.

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
#>   Fitting algorithm: ipu
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

Since the fitted weights contain decimals, we need to integerise them in
order to generate a synthetic population because it wouldn’t make sense
to create half a person. The package provides `ml_integerise()` to help
you with this task. `ml_integerise()` returns a `ml_fit` object with a
field that contains integerised weights.

``` r
int_fit <- ml_integerise(fit, algorithm = "trs")
int_fit
#> An object of class ml_fit
#>   Fitting algorithm: ipu
#>   Success: TRUE
#>   Residuals (absolute): min = -6.41906e-05, max = 0
#>   Integerisation algorithm: trs
#>     Relative diff. to fitted weights: 0.39 %
#>   Flat problem:
#>   An object of class flat_ml_fit_problem
#>     Dimensions: 5 groups, 8 target values
#>     Model matrix type: separate
#>     Original fitting problem:
#>     An object of class ml_problem
#>       Reference sample: 23 observations
#>       Control totals: 1 at individual, and 1 at group level
```

You can use `ml_evaluate()` to access the fit and the integerised fitted
weights across different matrices such as the G statistic, sum of
squared errors, standardised root mean squared errors, and R2.

``` r
dplyr::bind_rows(ml_evaluate(fit), ml_evaluate(int_fit))
#> # A tibble: 2 × 11
#>   fitting_algorithm success iterations      tol min_weight max_weight    gstat           sse       srmse    r2 integerisation_algorithm
#>   <chr>             <lgl>        <int>    <dbl>      <dbl>      <dbl>    <dbl>         <dbl>       <dbl> <dbl> <chr>                   
#> 1 ipu               TRUE           873 0.000001       1.36       27.8 5.20e-11 0.00000000795 0.000000554 1.00  <NA>                    
#> 2 ipu               TRUE           873 0.000001       1          28   5.66e- 2 3.00          0.0108      0.999 trs
```

Lastly, `ml_replicate()`, as its name suggests, helps to replicate the
reference sample based on the integerised weights.

``` r
syn_pop <- ml_replicate(int_fit)
syn_pop
#> # A tibble: 259 × 5
#>     HHNR   PNR  APER CAR   WKSTAT
#>    <int> <int> <int> <chr> <chr> 
#>  1     1     1     3 0     1     
#>  2     1     2     3 0     2     
#>  3     1     3     3 0     3     
#>  4     2     4     3 0     1     
#>  5     2     5     3 0     2     
#>  6     2     6     3 0     3     
#>  7     3     7     2 0     1     
#>  8     3     8     2 0     3     
#>  9     4     9     2 0     1     
#> 10     4    10     2 0     3     
#> # … with 249 more rows
```

## Example - multiple zones

This example is almost identical to the previous example, except we are
creating sub-fitting problems based on zones. `ml_problem()` has the
`geo_hierarchy` argument, where it lets you specify a geographical
hierarchy, a `data.frame` with two columns: `region` and `zone`. To put
it simply, a zone can only belong to one region. The image below shows
an example of that, where the orange patch is a zone that is within the
green region.

![](https://user-images.githubusercontent.com/17020181/113852241-afed5580-97df-11eb-80ed-2b458e8fcbda.png)

When `geo_hierarchy` is validly specified, `ml_problem()` would return a
list of fitting problems, one fitting problem per zone. Each fitting
problem will contain only relevant subsets of the reference sample and
control totals for its zone. Basically, the reference sample is a
population survey sample taken at a regional level and the control
totals should be at a zonal level.

``` r
ref_sample <- tibble::tribble(
  ~HHNR, ~PNR, ~APER, ~HH_VAR, ~P_VAR, ~REGION,
      1,    1,     3,       "1",      "1",       1,
      1,    2,     3,       "1",      "2",       1,
      1,    3,     3,       "1",      "3",       1,
      2,    4,     2,       "1",      "1",       1,
      2,    5,     2,       "1",      "3",       1,
      3,    6,     3,       "1",      "1",       1,
      3,    7,     3,       "1",      "1",       1,
      3,    8,     3,       "1",      "2",       1,
      4,    9,     3,       "2",      "1",       1,
      4,   10,     3,       "2",      "3",       1,
      4,   11,     3,       "2",      "3",       1,
      5,   12,     3,       "2",      "2",       1,
      5,   13,     3,       "2",      "2",       1,
      5,   14,     3,       "2",      "3",       1,
      6,   15,     2,       "2",      "1",       1,
      6,   16,     2,       "2",      "2",       1,
      7,   17,     5,       "2",      "1",       1,
      7,   18,     5,       "2",      "1",       1,
      7,   19,     5,       "2",      "2",       1,
      7,   20,     5,       "2",      "3",       1,
      7,   21,     5,       "2",      "3",       1,
      8,   22,     2,       "2",      "1",       1,
      8,   23,     2,       "2",      "2",       1,
      9,   24,     3,       "1",      "1",       2,
      9,   25,     3,       "1",      "2",       2,
      9,   26,     3,       "1",      "3",       2,
     10,   27,     2,       "1",      "1",       2,
     10,   28,     2,       "1",      "3",       2,
     11,   29,     3,       "1",      "1",       2,
     11,   30,     3,       "1",      "1",       2,
     11,   31,     3,       "1",      "2",       2,
     12,   32,     3,       "2",      "1",       2,
     12,   33,     3,       "2",      "3",       2,
     12,   34,     3,       "2",      "3",       2,
     13,   35,     3,       "2",      "2",       2,
     13,   36,     3,       "2",      "2",       2,
     13,   37,     3,       "2",      "3",       2,
     14,   38,     2,       "2",      "1",       2,
     14,   39,     2,       "2",      "2",       2,
     15,   40,     5,       "2",      "1",       2,
     15,   41,     5,       "2",      "1",       2,
     15,   42,     5,       "2",      "2",       2,
     15,   43,     5,       "2",      "3",       2,
     15,   44,     5,       "2",      "3",       2,
     16,   45,     2,       "2",      "1",       2,
     16,   46,     2,       "2",      "2",       2
  )


hh_ctrl <- tibble::tribble(
  ~ZONE, ~HH_VAR, ~N,
  1, "1", 35 * 0.7,
  1, "2", 65 * 0.7,
  2, "1", 35 * 0.5,
  2, "2", 65 * 0.5,
  3, "1", 35 * 0.9,
  3, "2", 65 * 0.9,
  4, "1", 35,
  4, "2", 65
)

ind_ctrl <- tibble::tribble(
  ~ZONE, ~P_VAR, ~N,
  1, "1", 91 * 0.7,
  1, "2", 65 * 0.7,
  1, "3", 104 * 0.7,
  2, "1", 91 * 0.5,
  2, "2", 65 * 0.5,
  2, "3", 104 * 0.5,
  3, "1", 91 * 0.9,
  3, "2", 65 * 0.9,
  3, "3", 104 * 0.9,
  4, "1", 91,
  4, "2", 65,
  4, "3", 104
)

geo_hierarchy <- tibble::tribble(
  ~REGION, ~ZONE,
  1, 1,
  1, 2,
  2, 3,
  2, 4
)

fitting_problems <- ml_problem(
    ref_sample = ref_sample,
    field_names = special_field_names(
      groupId = "HHNR", individualId = "PNR", count = "N",
      zone = "ZONE", region = "REGION"
    ),
    group_controls = list(hh_ctrl),
    individual_controls = list(ind_ctrl),
    geo_hierarchy = geo_hierarchy
  )
#> Creating a list of fitting problems by zone
```

``` r
library(purrr)

fit_algos <- eval(formals(ml_fit)$algorithm)
int_algos <- eval(formals(ml_integerise)$algorithm)

crossed_combs <- cross3(fitting_problems, fit_algos, int_algos)

fitted_problems <- map(crossed_combs, ~ 
  .x[[1]] %>%
    ml_fit(algorithm = .x[[2]]) %>%
    ml_integerise(algorithm = .x[[3]])
)
```

``` r
library(dplyr)
result <- map_dfr(fitted_problems, ~ ml_evaluate(.x)) %>%
  mutate(id = 1:n())

# find the best combination of each zone based on their G statistic.
best_combs <- result %>%
    group_by(zone) %>% 
    slice(which.min(srmse))

best_combs %>%
  knitr::kable()
```

| fitting_algorithm | integerisation_algorithm | success | iterations |   tol | zone | min_weight | max_weight |     gstat |       sse |     srmse |        r2 |  id |
|:------------------|:-------------------------|:--------|-----------:|------:|:-----|-----------:|-----------:|----------:|----------:|----------:|----------:|----:|
| entropy_o         | round                    | TRUE    |        123 | 1e-06 | 1    |          2 |         18 | 0.0314574 | 1.4799997 | 0.0107948 | 0.9989410 |  33 |
| dss               | trs                      | TRUE    |          6 | 1e-06 | 2    |          1 |         12 | 0.0293032 | 0.9999999 | 0.0124226 | 0.9985975 |  22 |
| dss               | trs                      | TRUE    |          6 | 1e-06 | 3    |          2 |         23 | 0.0152005 | 0.9199999 | 0.0066196 | 0.9996018 |  23 |
| dss               | round                    | TRUE    |          6 | 1e-06 | 4    |          3 |         26 | 0.0081511 | 0.9999999 | 0.0062113 | 0.9996494 |  40 |

``` r
library(ggplot2)

result %>%
  ggplot(aes(x = zone, y = srmse, fill = ifelse(id %in% best_combs$id, "Best", "Not best"))) +
  geom_col() +
  facet_grid(integerisation_algorithm ~ fitting_algorithm, scales = "free_x") +
  labs(fill = "", y = "Standardised RMSE")
```

<img src="man/figures/README-plot-eval-result-1.png" width="100%" />

``` r
map(fitted_problems[best_combs$id], ~ ml_replicate(.x))
#> [[1]]
#> # A tibble: 182 × 5
#>     HHNR   PNR  APER HH_VAR P_VAR
#>    <int> <int> <dbl> <chr>  <chr>
#>  1     1     1     3 1      1    
#>  2     1     2     3 1      2    
#>  3     1     3     3 1      3    
#>  4     2     4     3 1      1    
#>  5     2     5     3 1      2    
#>  6     2     6     3 1      3    
#>  7     3     7     3 1      1    
#>  8     3     8     3 1      2    
#>  9     3     9     3 1      3    
#> 10     4    10     3 1      1    
#> # … with 172 more rows
#> 
#> [[2]]
#> # A tibble: 131 × 5
#>     HHNR   PNR  APER HH_VAR P_VAR
#>    <int> <int> <dbl> <chr>  <chr>
#>  1     1     1     3 1      1    
#>  2     1     2     3 1      2    
#>  3     1     3     3 1      3    
#>  4     2     4     3 1      1    
#>  5     2     5     3 1      2    
#>  6     2     6     3 1      3    
#>  7     3     7     3 1      1    
#>  8     3     8     3 1      2    
#>  9     3     9     3 1      3    
#> 10     4    10     3 1      1    
#> # … with 121 more rows
#> 
#> [[3]]
#> # A tibble: 235 × 5
#>     HHNR   PNR  APER HH_VAR P_VAR
#>    <int> <int> <dbl> <chr>  <chr>
#>  1     1     1     3 1      1    
#>  2     1     2     3 1      2    
#>  3     1     3     3 1      3    
#>  4     2     4     3 1      1    
#>  5     2     5     3 1      2    
#>  6     2     6     3 1      3    
#>  7     3     7     3 1      1    
#>  8     3     8     3 1      2    
#>  9     3     9     3 1      3    
#> 10     4    10     3 1      1    
#> # … with 225 more rows
#> 
#> [[4]]
#> # A tibble: 261 × 5
#>     HHNR   PNR  APER HH_VAR P_VAR
#>    <int> <int> <dbl> <chr>  <chr>
#>  1     1     1     3 1      1    
#>  2     1     2     3 1      2    
#>  3     1     3     3 1      3    
#>  4     2     4     3 1      1    
#>  5     2     5     3 1      2    
#>  6     2     6     3 1      3    
#>  7     3     7     3 1      1    
#>  8     3     8     3 1      2    
#>  9     3     9     3 1      3    
#> 10     4    10     3 1      1    
#> # … with 251 more rows
```

## Powered by

-   [`grake`](https://krlmlr.github.io/grake/): A reimplementation of
    generalized raking ([Deville and Särndal,
    1992](https://amstat.tandfonline.com/doi/abs/10.1080/01621459.1992.10475217);
    [Deville, Särndal and Sautory,
    1993](https://www.tandfonline.com/doi/abs/10.1080/01621459.1993.10476369))

## Related work

-   [`wrswoR`](https://krlmlr.github.io/wrswoR/): An implementation of
    fast weighted random sampling without replacement ([Efraimidis and
    Spirakis,
    2006](https://www.sciencedirect.com/science/article/pii/S002001900500298X))
-   [`mangow`](https://krlmlr.github.io/mangow/): Embed the Gower
    distance metric in L1
-   [`RANN.L1`](https://github.com/jefferislab/RANN/tree/master-L1):
    k-nearest neighbors using the L1 metric

### Where is `MultiLeveLIPF`?

From version `0.4.0` onwards the package is now to be known as `mlfit`.
If you would like to install any version that is older than `0.4.0`
please use:

``` r
# See https://github.com/mlfit/mlfit/releases for the releases that are available
# To install a certain branch or commit or tag, append it to the repo name, after an @:
devtools::install_github("mlfit/mlfit@v0.3-7")
```

Note that, all versions prior to `0.4.0` should be used as
`MultiLeveLIPF` not `mlfit`.

## Citation

To cite package ‘mlfit’ in publications use:

Kirill Müller and Amarin Siripanich (2021). mlfit: Iterative
Proportional Fitting Algorithms for Nested Structures.
<https://mlfit.github.io/mlfit/>, <https://github.com/mlfit/mlfit>.

A BibTeX entry for LaTeX users is

    @Manual{,
      title = {mlfit: Iterative Proportional Fitting Algorithms for Nested Structures},
      author = {Kirill Müller and Amarin Siripanich},
      year = {2021},
      note = {https://mlfit.github.io/mlfit/, https://github.com/mlfit/mlfit},
    }

## Used in

-   Casati, D., Müller, K., Fourie, P. J., Erath, A., & Axhausen, K. W.
    (2015). Synthetic population generation by combining a hierarchical,
    simulation-based approach with reweighting by generalized raking.
    Transportation Research Record, 2493(1), 107-116.
-   Bösch, P. M., Müller, K., & Ciari, F. (2016). The IVT 2015 baseline
    scenario. In 16th Swiss Transport Research Conference (STRC 2016).
    16th Swiss Transport Research Conference (STRC 2016).
-   Müller, K. (2017). A generalized approach to population synthesis
    (Doctoral dissertation, ETH Zurich).
-   Ilahi, A., & Axhausen, K. W. (2018). Implementing Bayesian network
    and generalized raking multilevel IPF for constructing population
    synthesis in megacities. In 18th Swiss Transport Research Conference
    (STRC 2018). STRC.
-   Ilahi, A., & Axhausen, K. W. (2019). Integrating Bayesian network
    and generalized raking for population synthesis in Greater Jakarta.
    Regional Studies, Regional Science, 6(1), 623-636.
-   Yameogo, B. F., Vandanjon, P. O., Gastineau, P., & Hankach, P.
    (2021). Generating a two-layered synthetic population for French
    municipalities: Results and evaluation of four synthetic
    reconstruction methods. JASSS-Journal of Artificial Societies and
    Social Simulation, 24, 27p.
-   Zhou, M., Li, J., Basu, R., & Ferreira, J. (2022). Creating
    spatially-detailed heterogeneous synthetic populations for
    agent-based microsimulation. Computers, Environment and Urban
    Systems, 91, 101717.
