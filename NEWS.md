# MultiLevelIPF 0.2-3 (2016-04-08)

- Enhance example.
- Use `rflow`.


Version 0.2-2 (2016-03-13)
===

- Include flat problem (group size = 1 for all groups).
- Move legacy format (IPAF) and related functions to `data-raw` directory.
- Toy examples now load with `readRDS()`.
- `format()` and `print()` methods for classes `fitting_problem`, `flat_ml_fit_problem` and `ml_fit`.
- New `toy_example()` allows easier access to bundled examples.
- Use `sampling::calib()` instead of `survey::grake()`, because the latter forcibly attaches `MASS`.


Version 0.2-1 (2016-03-02)
===

- Allow problems with individual-only controls.
- Use factors internally.
- Check for correspondence of levels between sample and controls.
- Check for `NA` values in controls.


Version 0.2 (2016-01-30)
===

- New functions `compute_margins()` and `margins_to_df()` for validation
- Support specification of prior weights in construction of fitting problems
- Use `survey::grake()` instead of `grake::calibWeights()`.
- Adapt to change of undocumented behavior in base R.
- Don't alter column names of controls if they are of type `data.table` (explicitly convert to `data.frame`)
- Proper handling of corner cases (reference sample with one row, and grand total controls and dummy controls with only one category)
- Allow character variables (in addition to factors) as control variables
- Explicit error message if reference sample is not sorted
- If name of count column in controls is not specified, it is determined automatically (with a message in verbose mode)
- Expansion of weights loads `Matrix` package if necessary
- Clarify documentation
- Straighten out imports, use `importFrom` instead of `::`



v0.1 (2015-05-26)
===

- new functions `fitting_problem`, `is.fitting_problem`, `special_field_names`
- all fitting functions now expect an object of class `fitting_problem` (as returned by the `fitting_problem` and `import_IPAF_problem` functions); former calls like `ml_fit(ref_sample, controls, field_names)` now need to be written as `ml_fit(fitting_problem(...))`

v0.0-14 (2015-04-15)
===

- use `grake` package instead of `laeken`
- new argument `ginv` to `ml_fit_dss`, passed down to calibWeights

v0.0-13 (2015-04-13)
===

- fix example for `ml_fit_dss`

v0.0-12 (2014-11-21)
===

- new function `ml_fit_dss` with an implementation very close to the paper by
  Deville et al. (1993); implementation in the `laeken` package

- normalize weights to get rid of precision problems

- allow partly uncontrolled attributes and controls without observations in the reference sample (with a warning, #24)

- better error reporting for non-factor controls and existence of group ID column

- improve warning and progress messages

v0.0-11 (2014-07-25)
===

- return correct weights -- regression introduced in v0.0.9

- rewrite transformation of weights using sparse matrices and a home-grown
  Moore-Penrose inverse for our (very special) transformation matrix (#17)

- warn on missing observations for nonzero controls (#20)

- `ml_fit_entropy_o` also returns flat weights

- allow arbitrary order in control total tables (#19)

- remove observations that correspond to zero-valued control totals, with warning;
  don't warn if no corresponding observations need to be removed (#16)

v0.0-10 (2014-07-04)
===

- support multiple controls at individual or group level, also detect conflicting
  control totals

- support fitting one-dimensional problems (where only group-level controls are given)

v0.0.9 (2014-06-19)
===

- new function `flatten_ml_fit_problem`: transform representation as returned
  by `import_IPAF_result` into a matrix, a control vector and a weights vector

- function `ml_fit_entropy_o`: use `BB::dfsane` instead of `BB::BBsolve` for
  solving the optimization problem; rename argument `BBsolve_args` to `dfsane_args`

- function `ml_fit`: new parameter `verbose`

- aggregate identical household types, implement prior weights (so far only
  internally)

v0.0.8 (2014-06-17)
===

- Add example for `ml_fit` (#11)

- allow additional arguments for the algorithms; `ml_fit_entropy_o` now accepts
  a named list `BBsolve_args` that contains additional arguments to `BB::BBsolve`

- Faster internal data preparation for `ml_fit_entropy_o`

v0.0.7 (2014-06-17)
===

- Fix dependency issues (#13, #14)

- Add example for `ml_fit_entropy_o` (#11)

- Print more helpful error message if control totals and reference sample
  categories do not overlap (#11)

v0.0.6 (2014-02-09)
===

- `import_IPAF_results` now returns a class of type `IPAF_results`
- New functions `ml_ipf` and `ml_ipf_entropy_o`, implementation does not yet
  return the same weights as the Python code
- Convert control columns to factors

v0.0.5 (2014-02-07)
===

- Fix importing configuration files with more than one control of any type
  and with comments in the control definition

- New parameter `config_name` to `import`, defaults to `config.xml`

v0.0.4 (2013-12-06)
===

- Parameter `all_weights` to `import` that allows importing also intermediate
  weights.  The output format of `import` has changed, the weights for each
  algorithm are now always a list of weight vectors, even in the default case
  `all_weights == FALSE` (#5).

v0.0.3 (2013-11-28)
===

- Import results of old Python code (#1).

v0.0.2 (2013-11-26)
===

- Initial setup
