# ---------------------------------------
# Author: Kirill Müller
#         ETH Zurich
# Original ginv implementation from MASS
# ---------------------------------------

#' Generalized Inverse of a Matrix using a custom tolerance or SVD implementation
#'
#' The \code{gginv} function creates a function that
#' calculates the Moore-Penrose generalized inverse of a matrix X using a
#' fixed tolerance value and a custom
#' implementation for computing the singular value decomposition.
#'
#' The \code{svd} argument is expected to adhere to the interface of
#' \code{base::\link[base]{svd}}. It will be called as \code{svd(x)} (with the
#' \code{nu} and \code{nv} arguments unset) and is expected to return a named
#' list with components \code{d}, \code{u} and \code{v}.
#'
#' @inheritParams MASS::ginv
#' @param svd A function that computes the singular value decomposition of a
#'   matrix
#'
#' @return A function that accepts one argument \code{X} that computes a MP
#'   generalized inverse matrix for it.
#'
#' @seealso \code{\link[MASS]{ginv}}, \code{\link[base]{svd}}
#'
#' @author Adapted implementation from the \code{MASS} package.
#'
#' @export
gginv <- function(tol = sqrt(.Machine$double.eps), svd = base::svd) {
  env <- new.env(parent = baseenv())
  env$svd <- svd
  ret <- eval(bquote({
    function(X) {
      X <- as.matrix(X)
      if (!is.numeric(X) && !is.complex(X)) {
        stop("'X' must be a numeric or complex matrix")
      }
      Xsvd <- svd(X)
      d <- Xsvd$d
      u <- Xsvd$u
      if (is.complex(X)) u <- Conj(u)
      v <- Xsvd$v

      Positive <- which(d > max(.(tol) * d[[1L]], 0))
      if (length(Positive) == length(d)) {
        v %*% ((1 / d) * t(u))
      } else if (length(Positive) == 0) {
        array(0, dim(X)[2:1])
      } else {
        v[, Positive, drop = FALSE] %*%
          ((1 / d[Positive]) * t(u[, Positive, drop = FALSE]))
      }
    }
  }))

  environment(ret) <- env
  ret
}

# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

#' Calibrate sample weights
#'
#' Calibrate sample weights according to known marginal population totals.
#' Based on initial sample weights, the so-called \emph{g}-weights are computed
#' by generalized raking procedures.
#' The final sample weights need to be computed by multiplying the resulting
#' \emph{g}-weights with the initial sample weights.
#'
#' @encoding utf8
#'
#' @param X a matrix of calibration variables.
#' @param d a numeric vector giving the initial sample (or design) weights.
#' @param totals a numeric vector of population totals corresponding to the
#'   calibration variables in \code{X}.
#' @param q a numeric vector of positive values accounting for
#'   heteroscedasticity.  Small values reduce the variation of the
#'   \emph{g}-weights.
#' @param method a character string specifying the calibration method to be
#'   used.  Possible values are \code{"linear"} for the linear method,
#'   \code{"raking"} for the multiplicative method known as raking and
#'   \code{"logit"} for the logit method.
#' @param bounds a numeric vector of length two giving bounds for the g-weights
#'   to be used in the logit method.  The first value gives the lower bound (which
#'   must be smaller than or equal to 1) and the second value gives the upper
#'   bound (which must be larger than or equal to 1).  If \code{NULL}, the
#'   bounds are set to \code{c(0, 10)}.
#' @param maxit a numeric value giving the maximum number of iterations.
#' @param ginv a function that computes the Moore-Penrose generalized
#'   inverse (default: an optimized version of \code{\link[MASS]{ginv}}). In
#'   some cases it is possible to speed up the process by using
#'   a function that computes a "regular" matrix inverse such as
#'   \code{{solve.default}}.
#' @param tol relative tolerance; convergence is achieved if the difference of
#'   all residuals (relative to the corresponding total) is smaller than this
#'   tolerance.
#' @param attributes should additional attributes (currently
#'   \code{success}, \code{iterations}, \code{method} and \code{bounds})
#'   be added to the result? If \code{FALSE} (default), a warning is given
#'   if convergence within the given relative tolerance could not be achieved.
#'
#' @return A numeric vector containing the \emph{g}-weights.
#'
#' @note This is a faster implementation of parts of
#' \code{\link[sampling]{calib}} from package \code{sampling}.  Note that the
#' default calibration method is raking and that the truncated linear method is
#' not yet implemented.
#'
#' @author Andreas Alfons, with improvements by Kirill Müller
#'
#' @references Deville, J.-C. and \enc{Särndal}{Saerndal}, C.-E. (1992)
#' Calibration estimators in survey sampling. \emph{Journal of the American
#' Statistical Association}, \bold{87}(418), 376--382.
#'
#' Deville, J.-C., \enc{Särndal}{Saerndal}, C.-E. and Sautory, O. (1993)
#' Generalized raking procedures in survey sampling. \emph{Journal of the
#' American Statistical Association}, \bold{88}(423), 1013--1020.
#'
#' @keywords survey
#'
#' @importClassesFrom Matrix sparseMatrix
#' @examples
#' obs <- 1000
#' vars <- 100
#' Xs <- matrix(runif(obs * vars), nrow = obs)
#' d <- runif(obs) / obs
#' totals <- rep(1, vars)
#' g <- dss(Xs, d, totals, method = "linear", ginv = solve)
#' g2 <- dss(Xs, d, totals, method = "raking")
#' @export
dss <- function(X, d, totals, q = NULL, method = c("raking", "linear", "logit"),
                bounds = NULL, maxit = 500, ginv = gginv(), tol = 1e-06,
                attributes = FALSE) {
  ## initializations and error handling
  d <- as.numeric(d)
  totals <- as.numeric(totals)
  haveNA <- c(
    any(is.na(X)), any(is.na(d)),
    any(is.na(totals)), !is.null(q) && any(is.na(q))
  )
  if (any(haveNA)) {
    argsNA <- c("'X'", "'d'", "'totals'", "'q'")[haveNA]
    stop(
      "missing values in the following arguments: ",
      paste(argsNA, collapse = ", ")
    )
  }
  n <- nrow(X) # number of rows
  if (length(d) != n) stop("length of 'd' not equal to number of rows in 'X'")
  p <- ncol(X) # number of columns
  if (length(totals) != p) {
    stop("length of 'totals' not equal to number of columns in 'X'")
  }
  if (is.null(q)) {
    q <- rep.int(1, n)
  } else {
    q <- as.numeric(q)
    if (length(q) != n) {
      stop("length of 'q' not equal to number of rows in 'X'")
    }
    if (any(is.infinite(q))) stop("infinite values in 'q'")
  }
  method <- match.arg(method)

  # function to determine whether the desired accuracy has
  # been reached (to be used in the 'while' loop)
  tolReached <- function(X, w, totals, tol) {
    #' @importMethodsFrom Matrix crossprod
    max(abs(crossprod(X, w) / totals - 1)) < tol
  }

  i <- 1L

  ## computation of g-weights
  if (method == "linear") {
    ## linear method (no iteration!)
    lambda <- ginv(crossprod(X * d * q, X)) %*% (totals - as.vector(t(d) %*% X))
    g <- 1 + q * as.vector(X %*% lambda) # g-weights
  } else {
    ## multiplicative method (raking) or logit method
    lambda <- matrix(0, nrow = p) # initial values
    if (method == "raking") {
      ## multiplicative method (raking)
      # some initial values
      g <- rep.int(1, n) # g-weights
      w <- d # sample weights
      ## iterations
      while (!any(is.na(g)) && !tolReached(X, w, totals, tol) && i <= maxit) {
        # here 'phi' describes more than the phi function in Deville,
        # Saerndal and Sautory (1993); it is the whole last term of
        # equation (11.1)
        phi <- crossprod(X, w) - totals
        dphi <- crossprod(X * w, X) # derivative of phi function (to be inverted)
        lambda <- lambda - ginv(dphi) %*% phi # update 'lambda'
        g <- exp(as.vector(X %*% lambda) * q) # update g-weights
        w <- g * d # update sample weights
        i <- i + 1L # increase iterator
      }
    } else {
      ## logit (L, U) method
      ## error handling for bounds
      if (is.null(bounds)) bounds <- c(0, 10)
      if (length(bounds) < 2) {
        stop("'bounds' must be a vector of length 2")
      } else {
        bounds <- bounds[1:2]
      }
      if (bounds[1] >= 1) stop("the lower bound must be smaller than 1")
      if (bounds[2] <= 1) stop("the upper bound must be larger than 1")
      ## some preparations
      A <- diff(bounds) / ((1 - bounds[1]) * (bounds[2] - 1))
      # function to bound g-weights
      getG <- function(u, bounds) {
        (bounds[1] * (bounds[2] - 1) + bounds[2] * (1 - bounds[1]) * u) /
          (bounds[2] - 1 + (1 - bounds[1]) * u)
      }
      ## some initial values
      g <- getG(rep.int(1, n), bounds) # g-weights
      # in the procedure, g-weights outside the bounds are moved to the
      # bounds and only the g-weights within the bounds are adjusted.
      # these duplicates are needed since in general they are changed in
      # each iteration while the original values are also needed
      X1 <- X
      d1 <- d
      totals1 <- totals
      q1 <- q
      g1 <- g
      indices <- 1:n
      # function to determine which g-weights are outside the bounds
      anyOutOfBounds <- function(g, bounds) {
        any(g < bounds[1]) || any(g > bounds[2])
      }
      ## iterations
      while (!any(is.na(g)) && (!tolReached(X, g * d, totals, tol) ||
        anyOutOfBounds(g, bounds)) && i <= maxit) {
        # if some of the g-weights are outside the bounds, these values
        # are moved to the bounds and only the g-weights within the
        # bounds are adjusted
        if (anyOutOfBounds(g, bounds)) {
          g[g < bounds[1]] <- bounds[1]
          g[g > bounds[2]] <- bounds[2]
          # values within the bounds
          tmp <- which(g > bounds[1] & g < bounds[2])
          if (length(tmp) > 0) {
            indices <- tmp
            X1 <- X[indices, , drop = FALSE]
            d1 <- d[indices]
            if (length(indices) < n) {
              totals1 <- totals -
                as.vector(t(g[-indices] * d[-indices]) %*% X[-indices, , drop = FALSE])
            }
            q1 <- q[indices]
            g1 <- g[indices]
          }
        }
        w1 <- g1 * d1 # current sample weights
        # here 'phi' describes more than the phi function in Deville,
        # Saerndal and Sautory (1993); it is the whole last term of
        # equation (11.1)
        phi <- crossprod(X1, w1) - totals1
        dphi <- crossprod(X1 * w1, X1) # derivative of phi function (to be inverted)
        lambda <- lambda - ginv(dphi) %*% phi # update 'lambda'
        # update g-weights
        u <- exp(A * as.vector(X1 %*% lambda) * q1)
        g1 <- getG(u, bounds)
        g[indices] <- g1
        i <- i + 1L # increase iterator
      }
    }
  }

  ## check whether procedure converged
  success <- !any(is.na(g)) && i <= maxit && tolReached(X, g * d, totals, tol)

  if (attributes) {
    g <- structure(g, success = success, iterations = i, method = method, bounds = bounds)
  } else {
    if (!success) {
      warning("No convergence", call. = FALSE)
    }
  }

  ## return g-weights
  return(g)
}
