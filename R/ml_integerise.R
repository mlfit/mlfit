#' Integerise fitted weights
#'
#' @description
#' This function integerise the fitted weights of a `ml_fit` object.
#'
#' @param ml_fit A `ml_fit` object created by the [ml_fit()] family.
#' @param verbose If `TRUE`, print diagnostic output.
#' @param algorithm Replication algorithm to use. "trs" is
#'  the 'Truncate, replicate, sample' integerisation algorithm proposed
#'  by Lovelace et al. (2013), "pp" is weighted sampling with
#'  replacement, and "round" is just simple rounding.
#' 
#' @references
#'      Lovelace, R., & Ballas, D. (2013). ‘Truncate, replicate, sample’:
#'          A method for creating integer weights for spatial microsimulation.
#'          Computers, Environment and Urban Systems, 41, 1-11.
#'
#' @return `ml_fit` object with a new `int_weights` field which contains
#'  an integer vector of integerised weights. The new weights correspond
#'  to each individual in the reference sample.
#'
#' @export
#'
#' @examples
#' path <- toy_example("Tiny")
#' fit <- ml_fit(ml_problem = readRDS(path), algorithm = "entropy_o")
#' ml_integerise(fit, algorithm = "trs")
ml_integerise <- function(ml_fit, algorithm = c("pp", "trs", "round"), verbose = FALSE) {
    .patch_verbose()
    message("Integerising the fitted weights")
    stopifnot(is_ml_fit(ml_fit))
    algorithm <- match.arg(algorithm)
    int_weights <- .get_int_fnc(algorithm)(weights = ml_fit$flat_weights)
    group_id <- ml_fit$flat$ml_problem$fieldNames$groupId
    number_of_persons_in_each_group <-
        ml_fit$flat$ml_problem$refSample[[group_id]] %>%
        table() %>%
        as.integer()
    individual_int_weights <-
        rep(int_weights, number_of_persons_in_each_group)
    message("Adding integerised weights to the input `ml_fit` object.")
    ml_fit$int_weights <- individual_int_weights
    ml_fit$int_algo <- algorithm
    return(ml_fit)
}

.get_int_fnc <- function(algorithm) {
    getFromNamespace(sprintf("int_%s", algorithm),
        envir = as.environment("package:mlfit")
    )
}

int_trs <- function(weights) {
    truncated <- trunc(weights)
    remainders <- weights - truncated
    deficit <- round(sum(weights) - sum(truncated))
    if (deficit != 0) {
        sampled_indexes <-
            sample_int_crank(length(weights),
                size = deficit,
                prob = remainders
            )
        truncated[sampled_indexes] <-
            truncated[sampled_indexes] + 1L
    }
    truncated
}

int_pp <- function(weights) {
    sample(
        length(weights),
        size = round(sum(weights)),
        prob = weights,
        replace = T
    ) %>% tabulate(nbins = length(weights))
}

int_round <- function(weights) {
    round(weights)
}

get_ml_integerise_algo <- function(ml_fit) {
    stopifnot(is_ml_fit(ml_fit))
    ml_fit$int_algo
}
