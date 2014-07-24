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
