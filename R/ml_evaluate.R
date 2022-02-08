#' Evaluate fitted weights
#'
#' @param x a fitted problem.
#' @param y a synthetic population generated from the fitted problem in `x`.
#' @param ... dots.
#'
#' @return a tibble with statistics.
#' @export
ml_evaluate <- function(x, y = NULL, ...) {
    stopifnot(is_ml_fit(x))
    if (!is.null(y)) stopifnot(is.data.frame(y))

    actual <- x$flat$target_values
    predicted <- actual + x$residuals

    tibble(
        fitting_algorithm = get_ml_fit_algo(x),
        success = x$success,
        iterations = x$iterations,
        tol = x$tol,
        zone = x$flat$ml_problem$zone,
        min_weight = min(x$weights),
        max_weight =  max(x$weights), 
        gstat = compute_gstat(actual, predicted)$stat,
        sse = compute_sse(actual, predicted),
        srmse = compute_srmse(actual, predicted),
        r2 = compute_r2(actual, predicted)
    )
    
}

#' g statistics
compute_gstat <- function(actual, predicted) {
    entropy::Gstat(actual, predicted)
}

#' sum of squared errors
#'
#' @examples
#' sse(1:5, 2:6)
compute_sse <- function(actual, predicted) {
    sum((actual - predicted)^2)
}

compute_rmse <- function(actual, predicted) {
    N <- length(actual)
    sse <- compute_sse(actual, predicted)
    sqrt((1 / N) * sse)
}

#' standardised RMSE
#'
#' @description
#' The SRMSE tends to decrease for larger sampling fractions, but a larger sample may still result in a larger SRMSE than a “good” smaller sample.
#'
compute_srmse <- function(actual, predicted) {
    N <- length(actual)
    rmse <- compute_rmse(actual, predicted)
    rmse / ((1 / N) * sum(actual))
}

#' relative errors
compute_mape <- function(actual, predicted) {
    mean(abs(compute_pe(actual, predicted)))
}

compute_pe <- function(actual, predicted) {
    (actual - predicted) / predicted
}

#' Total sum of squares
compute_tss <- function(x) {
    sum((x - mean(x))^2)
}

#' R-squared
compute_r2 <- function(actual, predicted) {
    sse <- compute_sse(actual, predicted)
    tss <- compute_tss(actual)
    1 - (sse / tss)
}

