#' Estimate weights using a given algorithm
#'
#' This function reweights a reference sample to match constraints given by
#' aggregate controls using an algorithm given as parameter.
#'
#' @param algorithm Algorithm to use
#' @param fitting_problem A fitting problem created by
#'   \code{\link{fitting_problem}}.
#' @param tol Tolerance, the algorithm has succeeded when all target values are
#'   reached within this tolerance.
#' @param verbose If \code{TRUE}, print diagnostic output.
#' @param ... Further parameters passed to the algorithm
#' @return An object of class \code{ml_fit}, essentially a named list.
#' @export
#' @examples
#' path <- toy_example("Tiny")
#' ml_fit(algorithm = "entropy_o", fitting_problem = readRDS(path))
ml_fit <- function(algorithm = c("entropy_o", "dss", "ipu", "hipf"),
                   fitting_problem, verbose = FALSE, ..., tol = 1e-6) {
  algorithm <- match.arg(algorithm)
  fun.name <- sprintf("ml_fit_%s", algorithm)
  if (!exists(fun.name))
    stop("Unknown algorithm:", algorithm)

  get(fun.name)(
    fitting_problem = fitting_problem,
    tol = tol,
    verbose = verbose, ...)
}

.check_is_fitting_problem <- function(fitting_problem) {
  if (!is.fitting_problem(fitting_problem)) {
    stop("Please create a fitting problem using the fitting_problem function.")
  }
}

#' @importFrom kimisc export.list
.patch_verbose <- function() {
  verbose <- get("verbose", parent.frame())
  if (!verbose) {
    export.list(list(message=function(...) invisible(NULL)),
      target.env=parent.frame())
  }
}

get_algo <- function(x) {
  other_classes <- grep("^ml_fit_", class(x), value = TRUE)
  if (length(other_classes) == 0L)
    "(unknown)"
  else
    paste(gsub("^ml_fit_", "", other_classes), collapse = ", ")
}

set_weights_success_and_residuals <- function(res, tol, iterations) {
  res$tol <- tol
  res$iterations <- as.integer(iterations)
  res$weights <- expand_weights(res$flat_weights, res$flat)

  res2 <- get_success_and_residuals(
    res$flat_weights %*% res$flat$ref_sample,
    res$flat$target_values,
    tol)

  res[names(res2)] <- res2
  res$residuals <- res$flat_weighted_values - res$flat$target_values
  res
}

get_success_and_residuals <- function(flat_weighted_values, target_values, tol) {
  res <- list()
  res$flat_weighted_values <- as.vector(flat_weighted_values)
  res$rel_residuals <- rel_residuals(res$flat_weighted_values, target_values)
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
