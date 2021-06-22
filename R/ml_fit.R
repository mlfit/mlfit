#' Estimate weights for a fitting problem
#'
#' @description
#' These functions reweight a reference sample to match constraints given by
#'   aggregate controls.
#'
#' @description
#' `ml_fit()` accepts an algorithm as argument and calls the
#'   corresponding function. This is useful if the result of multiple algorithms
#'   are compared to each other.
#'
#' @param algorithm Algorithm to use
#' @param ml_problem A fitting problem or a list of fitting problems created by
#'   [ml_problem()] or returned by [flatten_ml_fit_problem()].
#' @param tol Tolerance, the algorithm has succeeded when all target values are
#'   reached within this tolerance.
#' @param verbose If `TRUE`, print diagnostic output.
#' @param ... Further parameters passed to the algorithm
#' @return All functions return an object of class `ml_fit`, which is
#'   a named list under the hood.  The class matches the function called,
#'   e.g., the return value of the `ml_fit_ipu` function also is of class
#'   `ml_fit_ipu`.
#' @export
#' @examples
#' path <- toy_example("Tiny")
#' fit <- ml_fit(ml_problem = readRDS(path), algorithm = "entropy_o")
#' fit
#' fit$weights
#' fit$tol
#' fit$iterations
#' fit$flat
#' fit$flat_weights
#' fit$residuals
#' fit$rel_residuals
#' fit$success
ml_fit <- function(ml_problem,
                   algorithm = c("entropy_o", "dss", "ipu", "hipf"),
                   verbose = FALSE, ..., tol = 1e-6) {
  ml_problem <- make_ml_list(ml_problem, "ml_problem")
  algorithm <- match.arg(algorithm)
  fun.name <- sprintf("ml_fit_%s", algorithm)
  if (!exists(fun.name)) {
    stop("Unknown algorithm:", algorithm)
  }
  fits <- lapply(ml_problem, get(fun.name), verbose = verbose, ..., tol = tol)
  if (length(fits) == 1) {
    return(fits[[1]])
  }
  fits
}

#' @export 
#' @rdname ml_problem
is_ml_problem <- make_is("ml_problem")

.check_is_ml_problem <- function(ml_problem) {
  if (!is_ml_problem(ml_problem)) {
    stop("Please create a fitting problem using the ml_problem function.")
  }
}

#' @importFrom kimisc export.list
.patch_verbose <- function() {
  verbose <- get("verbose", parent.frame())
  if (!verbose) {
    export.list(
      list(message = function(...) invisible(NULL)),
      target.env = parent.frame()
    )
  } else {
    export.list(
      list(message = new_timed_message()),
      target.env = parent.frame()
    )
  }
}

new_timed_message <- function() {
  start_time <- Sys.time()

  function(...) {
    current_time <- Sys.time() - start_time
    message(hms::as.hms(current_time), ": ", ...)
  }
}

get_algo <- function(x) {
  other_classes <- grep("^ml_fit_", class(x), value = TRUE)
  if (length(other_classes) == 0L) {
    "(unknown)"
  } else {
    paste(gsub("^ml_fit_", "", other_classes), collapse = ", ")
  }
}

#' @rdname ml_fit
#' @aliases NULL
#' @usage NULL
#' @return
#' All returned objects contain at least the following components, which can be
#' accessed with `$` or `[[`:
set_weights_success_and_residuals <- function(res, flat, flat_weights,
                                              tol, iterations) {

  #' - `weights`: Resulting weights, compatible to the original reference sample
  res$weights <- expand_weights(flat_weights, flat)
  #' - `tol`: The input tolerance
  res$tol <- tol
  #' - `iterations`: The actual number of iterations required to obtain the result
  res$iterations <- as.integer(iterations)

  #' - `flat`: The flattened fitting problem, see `flatten_ml_fit_problem()`
  res$flat <- flat
  #' - `flat_weights`: Weights in terms of the flattened fitting problem
  res$flat_weights <- flat_weights

  res2 <- get_success_and_residuals(
    flat_weights %*% flat$ref_sample,
    flat$target_values,
    tol
  )

  res[names(res2)] <- res2
  #' - `residuals`: Absolute residuals
  res$residuals <- res$flat_weighted_values - flat$target_values
  res
}

#' @rdname ml_fit
#' @aliases NULL
#' @usage NULL
#' @return
get_success_and_residuals <- function(flat_weighted_values, target_values, tol) {
  res <- list()
  res$flat_weighted_values <- as.vector(flat_weighted_values)
  #' - `rel_residuals`: Relative residuals
  res$rel_residuals <- rel_residuals(res$flat_weighted_values, target_values)
  #' - `success`: Are the residuals within the tolerance?
  res$success <- is_abs_within_tol(res$rel_residuals, tol)
  res
}


tol_reached <- function(last_weights, weights, tol) {
  is_abs_within_tol(rel_residuals(last_weights, weights), tol)
}

rel_residuals <- function(x, y) {
  nonzero <- y != 0
  x <- x[nonzero]
  y <- y[nonzero]
  x / y - 1
}

is_abs_within_tol <- function(x, tol) {
  max(abs(x)) < tol
}

# S3 ----------------------------------------------------------------------

#' @export
format.ml_fit <- function(x, ...) {
  c(
    "An object of class ml_fit",
    "  Algorithm: " %+% get_algo(x),
    "  Success: " %+% x$success,
    "  Residuals (absolute): min = " %+% format(min(x$residuals), ...) %+%
      ", max = " %+% format(max(x$residuals), ...),
    "  Flat problem:",
    "  " %+% format(x$flat)
  )
}

#' @export
print.ml_fit <- default_print
