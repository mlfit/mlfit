#' Replicate records in a reference sample based on its fitted weights
#' 
#' @description
#' These functions replicate each entry in a reference sample based on its fitted weights.
#' 
#' `ml_replicate()` accepts a replication method as argument and calls the
#' corresponding function. This is useful if the result of multiple 
#' replication algorithms are compared to each other, or to generate a full synthetic
#' population based on the result of a `ml_fit` object.
#' 
#' @param algorithm Replication algorithm to use
#' @param ml_fit A `ml_fit` object created by the [ml_fit()] family. 
#' @param verbose If `TRUE`, print diagnostic output.
#' @param ... Further parameters passed to the algorithm
#' 
#' @return All functions return a data.frame.
#' 
#' @export
#' @examples
#' path <- toy_example("Tiny")
#' fit <- ml_fit(algorithm = "entropy_o", fitting_problem = readRDS(path)) 
#' syn_pop <- ml_replicate(algorithm = "trs", fit)
ml_replicate <- function(algorithm = c("pp", "trs", "round"), ml_fit, verbose = FALSE, ...) {
    algorithm <- match.arg(algorithm)
    fun.name <- sprintf("int_%s", algorithm)
    if (!exists(fun.name)) {
        stop("Unknown replication algorithm:", algorithm)
    }
    
    message("Replicate using '", algorithm, "' algorithm")
    group_id <- ml_fit$flat$fitting_problem$fieldNames$groupId
    count_col <- ml_fit$flat$fitting_problem$fieldNames$count

    message("Extracting fitted weights of each group")
    number_of_persons_in_each_group <- 
        ml_fit$flat$fitting_problem$refSample[[group_id]] %>%
        table() %>% 
        as.integer()
    weights <-
        ml_fit$weights[!duplicated(ml_fit$flat$fitting_problem$refSample[[group_id]])]

    message("Integerising the fitted weights")   
    integerised_weights <- get(fun.name)(weights = weights)

    message("Duplicating the reference sample")
    replications <- 
        rep(integerised_weights, number_of_persons_in_each_group) %>% 
        {rep(1:nrow(ml_fit$flat$fitting_problem$refSample), .)}
    replicated_ref_sample <- 
        dplyr::slice(ml_fit$flat$fitting_problem$refSample, replications) %>%
        tibble::as_tibble()
    
    message("Done!")
    replicated_ref_sample
}

.check_is_ml_fit <- function(ml_fit) {
    if (!is.ml_fit(ml_fit)) {
        stop("Please create a ml_fit object using one of the `ml_fit` functions.")
    }
}

#' @export
#' @rdname ml_fit
#' @param x An object
is.ml_fit <- make_is("ml_fit")

int_trs <- function(weights) {
    truncated <- trunc(weights)
    remainders <- weights - truncated
    deficit <- round(sum(weights) - sum(truncated))
    if (deficit != 0) {
        sampled_indexes <-
            sample(length(weights),
                size = deficit,
                replace = FALSE,
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