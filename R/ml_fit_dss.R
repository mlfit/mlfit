#' \code{ml_fit_dss} uses generalized raking and calls \code{grake::\link[grake]{dss}} internally.
#'
#' @rdname ml_fit
#' @param method Calibration method, one of \code{"raking"} (default),
#'   \code{"linear"}, or \code{"logit"}
#' @param ginv Function that computes the Moore-Penrose pseudoinverse.
#' @references Deville, J.-C. and \enc{Särndal}{Saerndal}, C.-E. (1992)
#' Calibration estimators in survey sampling. \emph{Journal of the American
#' Statistical Association}, \bold{87 } (418), 376--382.
#'
#' Deville, J.-C., \enc{Särndal}{Saerndal}, C.-E. and Sautory, O. (1993)
#' Generalized raking procedures in survey sampling. \emph{Journal of the
#' American Statistical Association}, \bold{88 } (423), 1013--1020.
#'
#' @seealso \code{\link[grake]{dss}}, \code{\link[grake]{gginv}}
#' @export
#' @examples
#' ml_fit_dss(fitting_problem = readRDS(path))
#' ml_fit_dss(fitting_problem = readRDS(path), ginv = solve)
ml_fit_dss <- function(fitting_problem,
                       method = c("raking", "linear", "logit"),
                       ginv = grake::gginv(),
                       tol = 1e-6,
                       verbose = FALSE) {
  .patch_verbose()

  flat <- as.flat_ml_fit_problem(fitting_problem, verbose = verbose)

  message("Calibrating")
  method <- match.arg(method)

  g <- grake::dss(
    X = flat$ref_sample,
    d = flat$weights,
    totals = flat$target_values,
    method = method,
    ginv = ginv,
    tol = tol,
    attributes = TRUE)
  weights.agg <- g * flat$weights

  message("Done!")
  res <- new_ml_fit_dss(
    list(
      flat = flat,
      flat_weights = weights.agg
    )
  )

  set_weights_success_and_residuals(res, tol, attr(g, "iterations"))
}

new_ml_fit_dss <- make_new(c("ml_fit_dss", "ml_fit"))
