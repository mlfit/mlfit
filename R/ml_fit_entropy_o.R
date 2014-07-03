#' Estimate weights using an entropy optimization approach
#'
#' This function reweights a reference sample to match constraints given by
#' aggregate controls using an entropy optimization method.
#'
#' @inheritParams ml_fit
#' @param dfsane_args Additional arguments (as a named list) passed to the
#'   \code{\link[BB]{dfsane}} function used internally for the optimization.
#' @return An object of classes \code{ml_fit_entropy_o} and \code{ml_fit},
#'   essentially a named list.
#' @seealso \code{\link[BB]{dfsane}}
#' @export
#' @examples
#' path <- system.file("extdata/minitoy", package="MultiLevelIPF")
#' ml_fit_entropy_o(ref_sample = import_IPAF_results(path))
ml_fit_entropy_o <- function(ref_sample, controls, field_names, verbose = FALSE,
                             dfsane_args = list()) {
  .patch_ml_fit_args()
  .patch_verbose()

  flat <- flatten_ml_fit_problem(ref_sample = ref_sample, controls = controls,
                                 field_names = field_names, verbose = verbose)

  par <- rep(0, length(flat$control_totals))
  dfsane_args$par <- par
  dfsane_args$fn <- dss.objective.m(x=flat$ref_sample, control.totals=flat$control_totals, F=exp, d=flat$weights)
  dfsane_args$control$M <- 1
  dfsane_args$control$trace <- verbose

  # Testing evaluation
  message("Testing evaluation of objective function")
  dfsane_args$fn(dfsane_args$par)

  message("Searching for solution of optimization problem")
  bbout <- do.call(BB::dfsane, dfsane_args)

  message("Computing reference sample weights")
  weights.agg <- dss.weights.from.lambda.m(x=flat$ref_sample, F=exp, d=flat$weights)(bbout$par) / flat$weights

  weights.ref_sample <- weights.agg[flat$rev_weights_map]

  message("Done!")
  structure(
    list(
      weights=unname(weights.ref_sample),
      success=(bbout$message == "Successful convergence"),
      residuals=(flat$ref_sample %*% weights.agg)[,1] - flat$control_totals,
      flat=flat,
      bbout=bbout
    ),
    class=c("ml_fit_entropy_o", "ml_fit")
  )
}

# Equation 2.1 in Deville et al. (1993)
dss.weights.from.lambda.m <- function(x, F, d) {
  function(lambda) {
    apply(x, 2, function(xk) {
      F(sum(xk * lambda))
    }) * d
  }
}

# left-hand side of equation 2.2
# (right-hand side is control totals)
dss.lhs.orig.m <- function(x, F, d) {
  function(lambda) {
    dss.lhs.matrix <- apply(x, 2, function(xk) {
      F(sum(xk * lambda)) * xk
    }) * rep(d, each = length(lambda))
    apply(dss.lhs.matrix, 1, sum)
  }
}

# left-hand side of equation 2.2 using weights from equation 2.1
# equivalent to dss.lhs.orig.m, but surprisingly a bit faster when using in the solver
dss.lhs.using.w.m <- function(x, F, d) {
  dss.weights.from.lambda <- dss.weights.from.lambda.m(x, F, d)
  dss.lhs.using.w.f <- function(lambda) {
    (x %*% dss.weights.from.lambda(lambda))[,1]
  }
}

dss.objective.m <- function(x, control.totals, F, d, dss.lhs.m=dss.lhs.using.w.m) {
  dss.lhs.orig.f <- dss.lhs.m(x, F, d)
  function(lambda) {
    dss.lhs.orig.f(lambda) - control.totals
  }
}