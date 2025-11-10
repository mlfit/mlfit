<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# mlfit 0.6.1.9016 (2025-11-10)

## Chore

- Auto-update from GitHub Actions.

  Run: https://github.com/mlfit/mlfit/actions/runs/17451567143

- Auto-update from GitHub Actions.

  Run: https://github.com/mlfit/mlfit/actions/runs/14636205729

## Continuous integration

- Use workflows for fledge (#109).

- Sync (#108).

- Use reviewdog for external PRs (#107).

- Cleanup and fix macOS (#106).

- Format with air, check detritus, better handling of `extra-packages` (#105).

- Enhance permissions for workflow (#104).

- Permissions, better tests for missing suggests, lints (#103).

- Only fail covr builds if token is given (#102).

- Always use `_R_CHECK_FORCE_SUGGESTS_=false` (#101).

- Correct installation of xml2 (#100).

- Explain (#99).

- Add xml2 for covr, print testthat results (#98).

- Fix (#97).

- Sync (#96).


# mlfit 0.6.1.9015 (2024-12-09)

## Continuous integration

- Avoid failure in fledge workflow if no changes (#95).


# mlfit 0.6.1.9014 (2024-12-08)

## Continuous integration

- Fetch tags for fledge workflow to avoid unnecessary NEWS entries (#94).


# mlfit 0.6.1.9013 (2024-12-07)

## Continuous integration

- Use larger retry count for lock-threads workflow (#93).


# mlfit 0.6.1.9012 (2024-11-28)

## Continuous integration

- Ignore errors when removing pkg-config on macOS (#92).


# mlfit 0.6.1.9011 (2024-11-27)

## Continuous integration

- Explicit permissions (#91).


# mlfit 0.6.1.9010 (2024-11-26)

## Continuous integration

- Use styler from main branch (#90).


# mlfit 0.6.1.9009 (2024-11-25)

## Continuous integration

- Need to install R on Ubuntu 24.04 (#89).

- Use Ubuntu 24.04 and styler PR (#87).

## Uncategorized

- PLACEHOLDER https://github.com/mlfit/mlfit/pull/16 (#16).


# mlfit 0.6.1.9008 (2024-11-22)

## Continuous integration

  - Correctly detect branch protection (#86).


# mlfit 0.6.1.9007 (2024-11-18)

## Continuous integration

  - Use stable pak (#85).


# mlfit 0.6.1.9006 (2024-11-11)

## Continuous integration

  - Latest changes (#84).


# mlfit 0.6.1.9005 (2024-10-28)

## Continuous integration

  - Install via R CMD INSTALL ., not pak (#83).
    
      - ci: Install via R CMD INSTALL ., not pak
    
      - ci: Bump version of upload-artifact action
    
      - ci: Use pkgdown branch
    
      - ci: Updates from duckdb
    
      - ci: Trigger run
    
      - ci: Trigger run


# mlfit 0.6.1.9004 (2024-08-31)

## Chore

  - Auto-update from GitHub Actions.
    
    Run: https://github.com/mlfit/mlfit/actions/runs/10425482923

  - Auto-update from GitHub Actions.
    
    Run: https://github.com/mlfit/mlfit/actions/runs/10200151419

  - Auto-update from GitHub Actions.
    
    Run: https://github.com/mlfit/mlfit/actions/runs/9728444637

  - Auto-update from GitHub Actions.
    
    Run: https://github.com/mlfit/mlfit/actions/runs/9691619038

## Continuous integration

  - Install local package for pkgdown builds.

  - Improve support for protected branches with fledge.

  - Improve support for protected branches, without fledge.

  - Sync with latest developments.

  - Use v2 instead of master.

  - Inline action.

  - Use dev roxygen2 and decor.

  - Fix on Windows, tweak lock workflow.

  - Avoid checking bashisms on Windows.

  - Better commit message.

  - Bump versions, better default, consume custom matrix.

  - Recent updates.


# mlfit 0.6.1.9003 (2023-10-09)

- Internal changes only.


# mlfit 0.6.1.9002 (2023-03-24)

- Internal changes only.


# mlfit 0.6.1.9001 (2023-02-17)

- Internal changes only.


# mlfit 0.6.1.9000 (2023-02-10)

## Bug fixes

### ml_replicate

- Getting a replication algorithm now guarantee to w… (#81).


# mlfit 0.6.1 (2023-02-09)

- Fixed the internal working of `ml_replicate()`. Getting a replication algorithm now is guaranteed to work even without the mlfit package in the current environment. This can be an fatal issue when mlfit is internally called by another package. The root cause of this is in the .get_int_fnc() that uses as.environment("package:mlfit").


# mlfit 0.6.0.9001 (2023-02-02)

## Chore

- Styler.


# mlfit 0.6.0 (2023-02-01)

- Soft deprecated the `prior_weights` argument. The name of a weight column in the reference sample to be used as prior weights should be specified using the `prior_weight` argument in `special_field_names()`.
- the `group_controls` and `individual_controls` arguments of `ml_problem()` now have `NULL` as their default value.
- Fixed `ml_problem()` not allowing single-level control when the `geo_hierarchy` argument is not `NULL`. (#78, @walkerke)
- It is now possible to use prior weights in a `geo_hierarchy` `ml_problem()`. Simply specify the name of the weight column in your reference sample using the `prior_weight` argument in `special_field_names()`. (#78, @walkerke)


# mlfit 0.5.3.9004 (2023-01-04)

## doc

### NEWS.md 

- Correct the latest news update.

### news 

- Fixed #76. (#77).



# mlfit 0.5.3.9003 (2023-01-04)

- Fixed inconsistence header levels and invalid release titles in NEWS.md (#76, @maelle)


# mlfit 0.5.3.9002 (2022-12-30)

- Internal changes only.


# mlfit 0.5.3.9001 (2022-12-24)

- Harmonize yaml formatting.

- Revert changes to matrix section.

- Reduce parallelism.

- Also check dev on cran-* branches.

- Update hash key for dev.

- Remove R 3.3.


# mlfit 0.5.3.9000 (2021-10-08)

# mlfit 0.5.3 (2021-10-08)

- Fixed the length of the `flat_weights` field of an `ml_fit` object when there are one or more entries that correspond to zero-valued controls in its `ml_problem` object. (#60)
- Upgraded to `testthat` 3rd edition.
- Replaced deprecated functions.


# mlfit 0.5.2 (2021-07-02)

- Address CRAN comments (#62).
- Improve documentation.
- Document the return values of `compute_margins()` and `margin_to_df()` (#62)
- Add a return tag for `compute_margins()`. (#62)
- Shorten the package title to less than 65 chars (#62).


# mlfit 0.5.1 (2021-06-24)

- Make a file check test more robust as per CRAN's suggestion.


# mlfit 0.5.0 (2021-06-24)

- `ml_problem()` gains a `geo_hierarchy` argument, which let the user specifies a region and zone table for creating a list of `ml_problem` objects based on zones. See the `README` page for an example. Printing `ml_problem` will also show its zone, if exists.
- Add a person ID column to Ye's example. (#57)
- Fix the package's URL (#56) and URLs used in the package.
- Add a GPL-3 license file and the copyright holder role to Kirill.
- Add more changes for first CRAN submission. These changes do not affect any functionalities.
- Add an example section to README.


# mlfit 0.4.2 (2021-06-18)

- Add more changes for first CRAN submission. These changes do not affect any functionalities.
- Add an example section to README


# mlfit 0.4.1 (2021-06-16)

- Prepare for first CRAN release (#51).
- Cite Bar-Gera et al. 2009 for `ml_fit_dss()` (#55).
- Rename `is.ml_problem()` and `is.ml_fit()` to `is_ml_problem()` and `is_ml_fit()`.


# mlfit 0.4.0 (2021-04-26)

- Rename `fitting_problem()` to `ml_problem()` to be consistent with the other main functions (`ml_fit()` and `ml_replicate()`) (#42).
- Convert toy examples from `fitting_problem` objects to `ml_problem` objects (#46).
- Rename `as.flat_ml_fit_problem()` to `as_flat_ml_fit_problem()` (#48)
- Document how to install the old MultiLevelIPF package (#49).
- Rename the package to 'mlfit' (#39).
- `ml_fit()` now uses `fitting_problem` as first argument to better support pipe workflows (#41).
- add `ml_replicate()` which helps to replicate the reference sample of a fitted problem.
- Inline code from grake package (#36, @asiripanich).
- Use tidy evaluation (#33, @asiripanich).


# mlfit 0.3.7.9000 (2021-03-26)

- Add `ml_replicate()` for replicating the reference sample of a fitted problem (#38).
- Fix the error when a level is missing from the reference sample (#32, @asiripanich).
- Fix test on Windows.
- Avoid converting sparse matrix to full matrix.
- If the controls contain values of zero for existing observations in the reference sample, the removal of these observations now works in all cases (#30).


# mlfit 0.3-7 (2018-01-22)

- Add overview in package documentation.
- Add examples to all functions.
- Explicitly document return values to `ml_fit()`.


# mlfit 0.3-6 (2017-03-28)

- Set default maximum number of iterations for HIPF to 2000.
- Convert documentation to Markdown.


# mlfit 0.3-5 (2016-09-03)

- Add "Driven by" and "Related work" sections to the README.


# mlfit 0.3-4 (2016-08-03)

- Use a sparse matrix for the flattened reference sample.


# mlfit 0.3-3 (2016-06-06)

- Status messages with `verbose = TRUE` are prepended with a time stamp.
- Fail if `NA` group ID found.


# mlfit 0.3-2 (2016-04-25)

- Reorganized and renamed internal datasets.


# mlfit 0.3-1 (2016-04-17)

- Fitting result contains `iterations` and `tol` members (#28).
- Fixed model matrix of "separate" type if only grand totals are given.
- `ml_fit()` gains `tol` argument, which determines the success of a fitting operation.
- `ml_fit` objects have new members `success`, `rel_residuals`, and `flat_weighted_values` (#28).
- HIPF and IPU stop iterating if tolerance is reached.
- IPU and HIPF abort iteration when the weights do not change measurably between two iterations (#27).


# mlfit 0.3 (2016-04-14)

- Features
    - New algoritms: HIPF (#2) and IPU.

- Interface
    - New `as.flat_ml_fit_problem()` is used to coerce input for the `ml_fit_` functions.
    - `format()` and `print()` methods for classes `fitting_problem`, `flat_ml_fit_problem` and `ml_fit`.
    - Flattened reference sample now contains observations in rows, and controls in columns (#26).
    - `flatten_ml_fit_problem()` gains new `model_matrix_type` argument that allows selecting an alternative model matrix building method where all cross-classifications are allocated to a column, regardless of overlaps. Flattened problems store the type of model matrix used, it is also shown with the `format()` and `print()` methods.

- Improvements
    - Reference sample doesn't need to be ordered by group ID anymore.
    - Remove `individualsPerGroup` special variable.
    - Allow problems with individual-only controls.
    - Check for correspondence of levels between sample and controls.
    - Check for `NA` values in controls.

- Technical changes
    - Use `grake` package again for calibration, because the alternatives are worse: `sampling` uses a too low tolerance, `survey` forcibly loads `MASS`, and `laeken` could work but is unrelated (which is the reason `grake` has been started in the first place).
    - Duplicate rows are kept in the reference sample.
    - Rename `control_totals` to `target_values`.
    - New `toy_example()` allows easier access to bundled examples, load with `readRDS()`.
    - Move legacy format (IPAF) and related functions to `data-raw` directory.
    - Use factors internally.

- Performance
    - Use `dplyr` functions instead of `aggregate()`.

- Tests
    - Specific test for households with the same signature.

- Documentation
    - Enhance example.
    - Include flat example problem (group size = 1 for all groups).

- Cleanup
    - Cleanup and split of `flatten_ml_fit_problem()`.


# mlfit 0.2 (2016-01-30)

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



# mlfit 0.1 (2015-05-26)

- new functions `fitting_problem`, `is.fitting_problem`, `special_field_names`
- all fitting functions now expect an object of class `fitting_problem` (as returned by the `fitting_problem` and `import_IPAF_problem` functions); former calls like `ml_fit(ref_sample, controls, field_names)` now need to be written as `ml_fit(fitting_problem(...))`

# mlfit 0.0-14 (2015-04-15)

- use `grake` package instead of `laeken`
- new argument `ginv` to `ml_fit_dss`, passed down to calibWeights

# mlfit 0.0-13 (2015-04-13)

- fix example for `ml_fit_dss`

# mlfit 0.0-12 (2014-11-21)

- new function `ml_fit_dss` with an implementation very close to the paper by
  Deville et al. (1993); implementation in the `laeken` package

- normalize weights to get rid of precision problems

- allow partly uncontrolled attributes and controls without observations in the reference sample (with a warning, #24)

- better error reporting for non-factor controls and existence of group ID column

- improve warning and progress messages

# mlfit 0.0-11 (2014-07-25)

- return correct weights -- regression introduced in # mlfit 0.0.9

- rewrite transformation of weights using sparse matrices and a home-grown
  Moore-Penrose inverse for our (very special) transformation matrix (#17)

- warn on missing observations for nonzero controls (#20)

- `ml_fit_entropy_o` also returns flat weights

- allow arbitrary order in control total tables (#19)

- remove observations that correspond to zero-valued control totals, with warning;
  don't warn if no corresponding observations need to be removed (#16)

# mlfit 0.0-10 (2014-07-04)

- support multiple controls at individual or group level, also detect conflicting
  control totals

- support fitting one-dimensional problems (where only group-level controls are given)

# mlfit 0.0.9 (2014-06-19)

- new function `flatten_ml_fit_problem`: transform representation as returned
  by `import_IPAF_result` into a matrix, a control vector and a weights vector

- function `ml_fit_entropy_o`: use `BB::dfsane` instead of `BB::BBsolve` for
  solving the optimization problem; rename argument `BBsolve_args` to `dfsane_args`

- function `ml_fit`: new parameter `verbose`

- aggregate identical household types, implement prior weights (so far only
  internally)

# mlfit 0.0.8 (2014-06-17)

- Add example for `ml_fit` (#11)

- allow additional arguments for the algorithms; `ml_fit_entropy_o` now accepts
  a named list `BBsolve_args` that contains additional arguments to `BB::BBsolve`

- Faster internal data preparation for `ml_fit_entropy_o`

# mlfit 0.0.7 (2014-06-17)

- Fix dependency issues (#13, #14)

- Add example for `ml_fit_entropy_o` (#11)

- Print more helpful error message if control totals and reference sample
  categories do not overlap (#11)

# mlfit 0.0.6 (2014-02-09)

- `import_IPAF_results` now returns a class of type `IPAF_results`
- New functions `ml_ipf` and `ml_ipf_entropy_o`, implementation does not yet
  return the same weights as the Python code
- Convert control columns to factors

# mlfit 0.0.5 (2014-02-07)

- Fix importing configuration files with more than one control of any type
  and with comments in the control definition

- New parameter `config_name` to `import`, defaults to `config.xml`

# mlfit 0.0.4 (2013-12-06)

- Parameter `all_weights` to `import` that allows importing also intermediate
  weights.  The output format of `import` has changed, the weights for each
  algorithm are now always a list of weight vectors, even in the default case
  `all_weights == FALSE` (#5).

# mlfit 0.0.3 (2013-11-28)

- Import results of old Python code (#1).

# mlfit 0.0.2 (2013-11-26)

- Initial setup
