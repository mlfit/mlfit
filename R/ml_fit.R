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
#' path <- toy_example("minitoy")
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

tol_reached <- function(last_weights, weights, tol) {
  weights <- weights[last_weights != 0]
  last_weights <- last_weights[last_weights != 0]
  all(abs(weights / last_weights - 1) < tol)
}

set_weights_success_and_residuals <- function(res, tol) {
  res$weights <- expand_weights(res$flat_weights, res$flat)
  res$flat_weighted_values <- as.vector(res$flat_weights %*% res$flat$ref_sample)
  res$residuals <- res$flat_weighted_values - res$flat$target_values
  res$rel_residuals <- res$flat_weighted_values/ res$flat$target_values - 1
  res$success <- all(abs(res$rel_residuals) < tol)
  res
}


# S3 ----------------------------------------------------------------------

#' @export
format.ml_fit <- function(x, ...) {
  c(
    "An object of class ml_fit",
    "  Algorithm: " %+% get_algo(x),
    "  Success: " %+% x$success,
    "  Residuals: min = " %+% format(min(x$residuals), ...) %+%
      ", max = " %+% format(max(x$residuals), ...),
    "  Flat problem:",
    "  " %+% format(x$flat)
  )
}

#' @export
print.ml_fit <- default_print
