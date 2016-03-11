#' Estimate weights using a given algorithm
#'
#' This function reweights a reference sample to match constraints given by
#' aggregate controls using an algorithm given as parameter.
#'
#' @param algorithm Algorithm to use
#' @param fitting_problem A fitting problem created by
#'   \code{\link{fitting_problem}} or \code{\link{import_IPAF_results}}.
#' @param verbose If \code{TRUE}, print diagnostic output.
#' @param ... Further parameters passed to the algorithm
#' @return An object of class \code{ml_fit}, essentially a named list.
#' @export
#' @examples
#' path <- toy_example("minitoy")
#' ml_fit(algorithm = "entropy_o", fitting_problem = import_IPAF_results(path))
ml_fit <- function(algorithm = c("entropy_o", "dss"),
                   fitting_problem, verbose = FALSE, ...) {
  algorithm <- match.arg(algorithm)
  fun.name <- sprintf("ml_fit_%s", algorithm)
  if (!exists(fun.name))
    stop("Unknown algorithm:", algorithm)

  get(fun.name)(
    fitting_problem = fitting_problem,
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
