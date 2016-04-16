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
#' path <- toy_example("minitoy")
#' ml_fit_entropy_o(fitting_problem = readRDS(path))
ml_fit_entropy_o <- function(fitting_problem, verbose = FALSE, tol = 1e-6,
                             dfsane_args = list()) {
  .patch_verbose()

  flat <- as.flat_ml_fit_problem(fitting_problem, verbose = verbose)

  t_ref_sample <- t(flat$ref_sample)

  par <- rep(0, length(flat$target_values))
  dfsane_args$par <- par
  dfsane_args$fn <- dss.objective.m(x=t_ref_sample, control.totals=flat$target_values, F=exp, d=flat$weights)
  dfsane_args$control$M <- 1
  dfsane_args$control$trace <- verbose
  dfsane_args$alertConvergence <- FALSE

  # Testing evaluation
  message("Testing evaluation of objective function")
  dfsane_args$fn(dfsane_args$par)

  message("Searching for solution of optimization problem")
  bbout <- do.call(BB::dfsane, dfsane_args)

  message("Computing reference sample weights")
  weights.agg <- dss.weights.from.lambda.m(x=t_ref_sample, F=exp, d=flat$weights)(bbout$par)

  message("Done!")
  res <- new_ml_fit_entropy_o(
    list(
      flat=flat,
      flat_weights=weights.agg,
      bbout=bbout
    )
  )

  set_weights_success_and_residuals(res, tol)
}

new_ml_fit_entropy_o <- make_new(c("ml_fit_entropy_o", "ml_fit"))

# Equation 2.1 in Deville et al. (1993)
dss.weights.from.lambda.m <- function(x, F, d) {
  function(lambda) {
    w <- apply(x, 2, function(xk) {
      F(sum(xk * lambda))
    })
    w * d
  }
}

# left-hand side of equation 2.2
# (right-hand side is control totals)
dss.lhs.orig.m <- function(x, F, d) {
  function(lambda) {
    dss.lhs.matrix <- apply(x, 2, function(xk) {
      F(sum(xk * lambda)) * xk
    })
    dss.lhs.matrix <- dss.lhs.matrix * rep(d, each = length(lambda))
    apply(dss.lhs.matrix, 1, sum)
  }
}

# left-hand side of equation 2.2 using weights from equation 2.1
# equivalent to dss.lhs.orig.m, but surprisingly a bit faster when using in the solver
dss.lhs.using.w.m <- function(x, F, d) {
  dss.weights.from.lambda <- dss.weights.from.lambda.m(x, F, d)
  function(lambda) {
    (x %*% dss.weights.from.lambda(lambda))[,1]
  }
}

dss.objective.m <- function(x, control.totals, F, d, dss.lhs.m=dss.lhs.using.w.m) {
  dss.lhs.f <- dss.lhs.m(x, F, d)
  function(lambda) {
    dss.lhs.f(lambda) - control.totals
  }
}
