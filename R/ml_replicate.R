#' Replicate records in a reference sample based on its fitted weights
#'
#' @description
#' This function replicates each entry in a reference sample based on its 
#' integerised fitted weights. This is useful for generating a full 
#' synthetic population based on the result of a `ml_fit` object. 
#' 
#' Note that, all individual and group ids of the synthetic population are
#' not the same as those in the original reference sample, and the total 
#' number of groups replicated is always very close to or equal the sum 
#' of the fitted group weights.
#'
#' @param ml_fit A `ml_fit` object created by the [ml_fit()] family.
#' @param verbose If `TRUE`, print diagnostic output.
#' @param .keep_original_ids If `TRUE`, the original individual and group
#'  ids of the reference sample will be kept with suffix '_old'.
#'
#' @return The function returns a replicated sample in data.frame
#'   in the format used in the reference sample of the input `ml_fit` object.
#'
#' @export
#' @examples
#' path <- toy_example("Tiny")
#' fit <- ml_fit(ml_problem = readRDS(path), algorithm = "entropy_o") %>%
#'  ml_integerise(algorithm = "trs")
#' syn_pop <- ml_replicate(fit)
#' syn_pop
ml_replicate <- function(ml_fit, verbose = FALSE, .keep_original_ids = FALSE) {
  .patch_verbose()
  stopifnot(is_ml_fit(ml_fit))

  if (is.null(ml_fit$int_weights)) {
    stop("There are no integerised weights in the input `ml_fit` object. ",
      "Please use `ml_integerise()` to integerise the fitted weights before calling this function.")
  }

  message("Duplicating the reference sample")
  replications <-
    rep(
      seq_len(nrow(ml_fit$flat$ml_problem$refSample)),
      ml_fit$int_weights
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
