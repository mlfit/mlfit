mlfit 0.4.1

## Cran Repository Policy

- [x] Reviewed CRP last edited 2021/05/25

## Test environments

- [x] Checked locally, R 4.1.0
- [x] Checked on CI system, R 4.1.0
- [x] Checked on win-builder, R devel

## R CMD check results on local R 4.1.0 and CI system

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

* This is a new release.

## R CMD check results on win-builder, R devel

- Failed with one error because the error message was in German, but the failed check was expecting the word 'file' in the returned error message. However, when we translated the error message from German to English the check result was as expected.

``` r
Running the tests in 'tests/test-all.R' failed.
Complete output:
  > library(testthat)
  > 
  > test_check("mlfit")
  Loading required package: mlfit
  == Failed tests ================================================================
  -- Failure (test-toy.R:9:3): Can enumerate toy examples ------------------------
  `toy_example("nonexisting-toy")` threw an error with unexpected message.
  Expected match: "file"
  Actual message: "path[1]=\"D:/temp/RtmpQLLWlD/RLIBS_708456d2480c/mlfit/extdata/nonexisting-toy.rds\": Das System kann die angegebene Datei nicht finden"
  Backtrace:
      x
   1. +-testthat::expect_error(toy_example("nonexisting-toy"), "file") test-toy.R:9:2
   2. | \-testthat:::quasi_capture(...)
   3. |   +-testthat:::.capture(...)
   4. |   | \-base::withCallingHandlers(...)
   5. |   \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
   6. \-mlfit::toy_example("nonexisting-toy")
   7.   \-base::normalizePath(file.path(root, paste0(name, ".rds")), mustWork = TRUE)
  
  [ FAIL 1 | WARN 0 | SKIP 0 | PASS 586 ]
  Error: Test failures
  Execution halted
* checking PDF version of manual ... OK
* checking for detritus in the temp directory ... OK
* DONE
Status: 1 ERROR, 2 NOTEs
```

- IPF is an acronymn for Iterative Proportional Fitting.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Amarin Siripanich <amarin.siri@gmail.com>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  IPF (35:50)

Found the following (possibly) invalid URLs:
  URL: https://krlmlr.github.io/mlfit (moved to https://krlmlr.github.io/mlfit/)
    From: DESCRIPTION
          man/mlfit-package.Rd
    Status: 200
    Message: OK
```

## Reverse dependencies

None.