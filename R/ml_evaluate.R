#' Evaluate fitted or integerised weights
#' 
#' This function evaluates sum of squared errors, standardised RMSE, 
#' G statistic, and R2 of the fitted weights of a `ml_fit` object. However,
#' if integerised weights are available, meaning `ml_integerise()` has 
#' been called on the `ml_fit` object, it will evaluate the integerised
#' weights instead.
#'
#' @param ml_fit a fitted problem of class `ml_fit`.
#'
#' @return a tibble containing various statistics of the weights.
#' @export
ml_evaluate <- function(ml_fit) {
    stopifnot(is_ml_fit(ml_fit))

    if (!is.null(ml_fit$int_weights)) {
        predicted_weight_col <- "int_weights"
        actual <- compute_margins(ml_fit$flat$ml_problem, ml_fit$weights) %>%
            margin_to_df() %>%
            dplyr::pull(..count..)
        predicted <- compute_margins(ml_fit$flat$ml_problem, ml_fit$int_weights) %>%
            margin_to_df() %>%
            dplyr::pull(..count..)
    } else {
        predicted_weight_col <- "weights"
        actual <- ml_fit$flat_weighted_values
        predicted <- actual + ml_fit$residuals
    }

    tibble(
        fitting_algorithm = get_ml_fit_algo(ml_fit),
        integerisation_algorithm = get_ml_integerise_algo(ml_fit),
        success = ml_fit$success,
        iterations = ml_fit$iterations,
        tol = ml_fit$tol,
        zone = ml_fit$flat$ml_problem$zone,
        min_weight = min(ml_fit[[predicted_weight_col]]),
        max_weight =  max(ml_fit[[predicted_weight_col]]), 
        gstat = compute_gstat(actual, predicted),
        sse = compute_sse(actual, predicted),
        srmse = compute_srmse(actual, predicted),
        r2 = compute_r2(actual, predicted)
    )
}

#' g statistics
compute_gstat <- function(actual, predicted) {
    predicted_pdf = predicted / sum(predicted)
    actual_pdf = actual / sum(actual)
    LR = ifelse(predicted_pdf > 0, log(predicted_pdf/actual_pdf), 0)
    KL = sum(predicted_pdf * LR)
    2 * sum(actual) * KL
}

#' sum of squared errors
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
#' Note from Kirill's thesis:
#' "The SRMSE tends to decrease for larger sampling fractions, 
#' but a larger sample may still result in a larger SRMSE than a “good” smaller sample."
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

