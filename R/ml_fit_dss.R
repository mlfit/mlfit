#' Estimate weights using generalized raking
#'
#' This function reweights a reference sample to match constraints given by
#' aggregate controls by means of generalized raking.
#'
#' Internally, \code{survey::\link[survey]{grake}} is called.
#'
#' @inheritParams ml_fit
#' @param method Calibration method, one of \code{"raking"} (default),
#'   \code{"linear"}, or \code{"logit"}
#' @param ginv Function that computes the Moore-Penrose pseudoinverse,
#'   currently only the default value \code{MASS::\link[MASS]{ginv}} is
#'   accepted.
#' @return An object of classes \code{ml_fit_dss} and \code{ml_fit},
#'   essentially a named list.
#' @references Deville, J.-C. and \enc{Särndal}{Saerndal}, C.-E. (1992)
#' Calibration estimators in survey sampling. \emph{Journal of the American
#' Statistical Association}, \bold{87}(418), 376--382.
#'
#' Deville, J.-C., \enc{Särndal}{Saerndal}, C.-E. and Sautory, O. (1993)
#' Generalized raking procedures in survey sampling. \emph{Journal of the
#' American Statistical Association}, \bold{88}(423), 1013--1020.
#'
#' @seealso \code{\link[survey]{grake}}
#' @export
#' @examples
#' path <- system.file("extdata/minitoy", package="MultiLevelIPF")
#' ml_fit_dss(fitting_problem = import_IPAF_results(path))
#' \dontrun{ml_fit_dss(fitting_problem = import_IPAF_results(path), ginv = solve)}
ml_fit_dss <- function(fitting_problem,
                       method = c("raking", "linear", "logit"),
                       ginv = MASS::ginv,
                       verbose = FALSE) {
  .patch_verbose()

  flat <- if (is.fitting_problem(fitting_problem)) {
    flatten_ml_fit_problem(fitting_problem = fitting_problem, verbose = verbose)
  } else {
    fitting_problem
  }

  message("Calibrating")
  method <- match.arg(method)
  calfun <- switch(method,
                   linear=survey::cal.linear,
                   raking=survey::cal.raking,
                   logit=survey::cal.logit)

  if (!identical(ginv, MASS::ginv)) {
    stop("Currently, only ginv = MASS::ginv is supported.", call. = FALSE)
  }

  g <- survey::grake(mm = t(flat$ref_sample), ww = flat$weights,
                     calfun = calfun, bounds = c(-Inf, Inf),
                     population = flat$control_totals,
                     epsilon = 1e-7, verbose = FALSE, maxit = 50)
  weights.agg <- g * flat$weights

  message("Done!")
  structure(
    list(
      weights=expand_weights(weights.agg, flat),
      success=TRUE,
      residuals = (flat$ref_sample %*% weights.agg)[,1] - flat$control_totals,
      flat=flat,
      flat_weights=weights.agg
    ),
    class=c("ml_fit_dss", "ml_fit")
  )
}
