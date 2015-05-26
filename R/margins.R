#' Compute margins for a weighting of a multi-level fitting problem
#'
#' This function allows checking a fit in terms of the original input data.
#'
#' @inheritParams ml_fit
#' @param weights A vector with one entry per row of the original reference
#'   sample
#' @return Margins of the same format as expected by the \code{controls}
#'   parameter of the \code{\link{fitting_problem}} function.
#' @seealso \code{\link{ml_fit}}
#' @export
compute_margins <- function(fitting_problem, weights, verbose = FALSE) {
  .check_is_fitting_problem(fitting_problem)
  ref_sample <- fitting_problem$refSample
  controls <- fitting_problem$controls
  field_names <- fitting_problem$fieldNames
  prior_weights <- fitting_problem$priorWeights

  .patch_verbose()

  if (length(weights) != nrow(ref_sample)) {
    stop("For weights, use a vector with the same length as the number of rows in the reference sample.")
  }

  message("Aggregating")
  plyr::llply(
    setNames(nm=names(controls)),
    function(control.type) {
      weights_df <- data.frame(
        ..w.. = if (control.type == "individual") {
          weights
        } else {
          weights / ref_sample[[field_names$individualsPerGroup]]
        })
      ref_sample_w <- cbind(weights_df, ref_sample)

      control.list <- controls[[control.type]]
      plyr::llply(
        control.list,
        control.type = control.type,
        function(control, control.type) {
          plyr::rename(
            plyr::count(
              ref_sample_w,
              setdiff(names(control), field_names[["count"]]),
              "..w.."
            ),
            c("freq" = field_names[["count"]])
          )
        }
      )
    }
  )
}
