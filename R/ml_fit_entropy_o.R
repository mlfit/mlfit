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
#' ml_fit_entropy_o(fitting_problem = import_IPAF_results(path))
ml_fit_entropy_o <- function(fitting_problem, verbose = FALSE,
                             dfsane_args = list()) {
  .patch_verbose()

  flat <- if (is.fitting_problem(fitting_problem)) {
    flatten_ml_fit_problem(fitting_problem = fitting_problem, verbose = verbose)
  } else {
    fitting_problem
  }

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
  weights.agg <- dss.weights.from.lambda.m(x=flat$ref_sample, F=exp, d=flat$weights)(bbout$par)

  weights.ref_sample <- as.vector(weights.agg %*% flat$reverse_weights_transform)

  message("Done!")
  structure(
    list(
      weights=unname(weights.ref_sample),
      success = (bbout$message == "Successful convergence"),
      residuals = (flat$ref_sample %*% weights.agg)[,1] - flat$control_totals,
      flat=flat,
      flat_weights=weights.agg,
      bbout=bbout
    ),
    class=c("ml_fit_entropy_o", "ml_fit")
  )
}

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
