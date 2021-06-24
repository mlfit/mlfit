#' Replicate records in a reference sample based on its fitted weights
#'
#' @description
#' This function replicates each entry in a reference sample based on its fitted
#' weights. This is useful if the result of multiple replication algorithms
#' are compared to each other, or to generate a full synthetic population
#' based on the result of a `ml_fit` object. Note that, all individual
#' and group ids of the synthetic population are not the same as those in
#' the original reference sample, and the total number of groups replicated
#' is always very close to or equal the sum of the fitted group weights.
#'
#' @param ml_fit A `ml_fit` object created by the [ml_fit()] family.
#' @param algorithm Replication algorithm to use. "trs" is
#'  the 'Truncate, replicate, sample' integerisation algorithm proposed
#'  by Lovelace et al. (2013), "pp" is weighted sampling with
#'  replacement, and "round" is just simple rounding.
#' @param verbose If `TRUE`, print diagnostic output.
#' @param .keep_original_ids If `TRUE`, the original individual and group
#'  ids of the reference sample will be kept with suffix '_old'.
#'
#' @references
#'      Lovelace, R., & Ballas, D. (2013). ‘Truncate, replicate, sample’:
#'          A method for creating integer weights for spatial microsimulation.
#'          Computers, Environment and Urban Systems, 41, 1-11.
#'
#' @return The function returns a replicated sample in data.frame 
#'   in the format used in the reference sample of the input `ml_fit` object.
#'
#' @export
#' @examples
#' path <- toy_example("Tiny")
#' fit <- ml_fit(ml_problem = readRDS(path), algorithm = "entropy_o")
#' syn_pop <- ml_replicate(fit, algorithm = "trs")
#' syn_pop
ml_replicate <- function(ml_fit, algorithm = c("pp", "trs", "round"), verbose = FALSE, .keep_original_ids = FALSE) {
  .patch_verbose()

  algorithm <- match.arg(algorithm)

  message("Replicate using '", algorithm, "' algorithm")
  group_id <- ml_fit$flat$ml_problem$fieldNames$groupId
  count_col <- ml_fit$flat$ml_problem$fieldNames$count

  message("Extracting fitted weights of each group")
  number_of_persons_in_each_group <-
    ml_fit$flat$ml_problem$refSample[[group_id]] %>%
    table() %>%
    as.integer()
  weights <-
    ml_fit$flat_weights

  message("Integerising the fitted weights")
  integerised_weights <- .get_int_fnc(algorithm)(weights = weights)

  message("Duplicating the reference sample")
  ind_integerised_weights <-
    rep(integerised_weights, number_of_persons_in_each_group)
  replications <-
    rep(
      seq_len(nrow(ml_fit$flat$ml_problem$refSample)),
      ind_integerised_weights
    )
  replicated_ref_sample <-
    dplyr::slice(ml_fit$flat$ml_problem$refSample, replications) %>%
    tibble::as_tibble()

  message("Assign new ids to individuals and groups")
  field_names <- ml_fit$flat$ml_problem$fieldNames
  replicated_ref_sample <-
    replicated_ref_sample %>%
    dplyr::group_by(
      .data[[field_names$groupId]],
      .data[[field_names$individualId]]
    ) %>%
    dplyr::mutate(..rep_id.. = 1:n()) %>%
    dplyr::group_by(
      .data[[field_names$groupId]],
      .data[["..rep_id.."]]
    ) %>%
    dplyr::mutate(..group_id.. = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$..group_id..) %>%
    dplyr::mutate(..ind_id.. = 1:n())

  replicated_ref_sample[[field_names$groupId]] <-
    replicated_ref_sample[["..group_id.."]]
  replicated_ref_sample[[field_names$individualId]] <-
    replicated_ref_sample[["..ind_id.."]]

  if (.keep_original_ids) {
    replicated_ref_sample[[paste0(field_names$groupId, "_old")]] <-
      replicated_ref_sample[[field_names$groupId]]
    replicated_ref_sample[[paste0(field_names$individualId, "_old")]] <-
      replicated_ref_sample[[field_names$individualId]]
  }

  message("Done!")
  tmp_cols <- grepl("^\\.\\.(.*)\\.\\.$", names(replicated_ref_sample))
  replicated_ref_sample[, !tmp_cols]
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
