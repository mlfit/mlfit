#' Return a flattened representation of a multi-level fitting problem instance
#'
#' This function transforms a multi-level fitting problem to a representation
#' more suitable for applying the algorithms:  A matrix with one row per controlled
#' attribute and one column per unique household, a weight vector with one weight
#' per household, and a control vector.
#'
#' @inheritParams ml_fit
#' @return An object of classes \code{flat_ml_fit_problem},
#'   essentially a named list.
#' @seealso \code{\link{ml_fit}}
#' @export
#' @examples
#' path <- system.file("extdata/minitoy", package="MultiLevelIPF")
#' flatten_ml_fit_problem(ref_sample = import_IPAF_results(path))
flatten_ml_fit_problem <- function(ref_sample, controls, field_names, verbose = FALSE) {
  .patch_ml_fit_args()
  .patch_verbose()

  message("Preparing controls")
  control.terms.list <- plyr::llply(
    controls,
    function(control.list) {
      control.columns <- plyr::llply(
        control.list,
        function(control) {
          control.names <- setdiff(colnames(control), field_names$count)
          control.levels <- vapply(
            control[control.names], function(f) length(levels(f)), integer(1))

          control.term <- paste(control.names[control.levels > 1], collapse=':')

          control.mm <- model.matrix(
            as.formula(sprintf("~%s-1", control.term)),
            control)

          list(
            term=control.term,
            control=(control[[field_names$count]] %*% control.mm)[1,]
          )
        }
      )
    }
  )

  control.formulae <- plyr::llply(
    control.terms.list,
    function(control.terms) {
      paste(plyr::laply(control.terms, `[[`, 'term'), collapse='+')
    }
  )

  message("Flattening reference sample")
  ref_sample_grp.mm <- as.data.frame(model.matrix(
    as.formula(sprintf("~%s+%s-1", field_names$groupId, control.formulae$group)),
    ref_sample))
  ref_sample_grp.agg <- ref_sample_grp.mm[c(TRUE, diff(ref_sample_grp.mm[[field_names$groupId]]) != 0), ]
  ref_sample_ind.mm <- as.data.frame(model.matrix(
    as.formula(sprintf("~%s+%s-1", field_names$groupId, control.formulae$individual)),
    ref_sample))
  ref_sample_ind.agg <- aggregate(as.formula(sprintf(".~%s", field_names$groupId)),
                                  ref_sample_ind.mm, FUN=sum)
  ref_sample.agg <- merge(ref_sample_ind.agg, ref_sample_grp.agg,
                          by=field_names$groupId)

  ref_sample.agg.agg <- aggregate(as.formula(sprintf("%s~.", field_names$groupId)),
                                  ref_sample.agg, FUN=identity)
  w <- vapply(ref_sample.agg.agg[, field_names$groupId], length, integer(1))

  ref_sample.agg.agg.m <- t(as.matrix(ref_sample.agg.agg[
    , setdiff(colnames(ref_sample.agg.agg), field_names$groupId)]))

  message("Flattening controls")
  control.totals.list <- plyr::llply(
    control.terms.list,
    function(control.terms) {
      plyr::laply(control.terms, `[[`, 'control', .drop=TRUE)
    }
  )
  control.totals <- unlist(unname(control.totals.list), use.names=TRUE)
  if (any(names(control.totals) != rownames(ref_sample.agg.agg.m))) {
    stop("  The following controls do not have any corresponding observation in the reference sample:\n    ",
         paste(setdiff(names(control.totals), rownames(ref_sample.agg.agg.m)), collapse=", "), "\n",
         "  The following categories in the reference sample do not have a corresponding control:\n    ",
         paste(setdiff(rownames(ref_sample.agg.agg.m), names(control.totals)), collapse=", "), "\n"
    )
  }

  message("Computing reverse weights map")
  agg_map <- (function(x) unlist(setNames(x, paste0(seq_along(x), "."))))(ref_sample.agg.agg[, field_names$groupId])
  agg_map_idx <- floor(as.numeric(names(agg_map)))
  rev_weights_map.agg <- agg_map_idx[match(ref_sample.agg[[field_names$groupId]], agg_map)]

  rev_weights_map <- rev_weights_map.agg[match(ref_sample[[field_names$groupId]], ref_sample.agg[[field_names$groupId]])]

  message("Done!")
  structure(
    list(
      ref_sample=ref_sample.agg.agg.m,
      weights=w,
      control_totals=control.totals,
      rev_weights_map=rev_weights_map
    ),
    class=c("flat_ml_fit_problem")
  )
}
