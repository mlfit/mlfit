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

  # Assume uniform prior weights
  prior_weights <- rep(1, nrow(ref_sample))

  message("Computing expected individuals-per-group ratio")
  IPG <- if (length(controls$group) * length(controls$individual) > 0) {
    sum(controls$individual[[1]][[field_names$count]]) / sum(controls$group[[1]][[field_names$count]])
  } else
    NULL

  message("Preparing controls")
  control.terms.list <- plyr::llply(
    setNames(nm=names(controls)),
    function(control.type) {
      control.list <- controls[[control.type]]
      control.columns <- plyr::llply(
        control.list,
        control.type = control.type,
        function(control, control.type) {
          control.and.count.names <- setNames(nm=colnames(control))
          control.names.unordered <- setdiff(control.and.count.names, field_names$count)
          control.names <- colnames(ref_sample)[colnames(ref_sample) %in% control.names.unordered]
          if (length(control.names) != length(control.names.unordered)) {
            stop("Control variable(s) not found: ",
                 paste0(setdiff(control.names.unordered, control.names), collapse = ", "))
          }
          control.levels <- vapply(
            control[control.names], function(f) length(levels(f)), integer(1))
          control.names <- control.names[control.levels > 1]

          new.control.names <- sprintf("%s_%s_", control.names, .control.type.abbrev(control.type))
          control.and.count.names[control.names] <- new.control.names
          colnames(control) <- control.and.count.names

          control.term <- paste0(new.control.names, collapse='*')

          control.mm <- model.matrix(
            as.formula(sprintf("~%s", control.term)),
            control)
          control.mm <- .rename.intercept(control.mm, control.type)

          list(
            control.names=control.names,
            new.control.names=new.control.names,
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

  # List of "individual" and "group"
  #   Each item contains a named vector: names are column names in the original
  #   dataset, values are new names in the mangled dataset
  control.names <- plyr::llply(
    control.terms.list,
    function(control.terms) {
      control.names <- unlist(plyr::llply(unname(control.terms), function(control.term)
        setNames(control.term$new.control.names, control.term$control.names)))
      control.names[!duplicated(control.names)]
    }
  )

  message("Preparing reference sample")
  stopifnot(is.numeric(ref_sample[[field_names$groupId]]))
  ref_sample_grp.mm <- as.data.frame(model.matrix(
    as.formula(sprintf("~%s+%s", field_names$groupId, control.formulae$group)),
    plyr::rename(ref_sample[c(field_names$groupId, names(control.names$group))], control.names$group)))
  ref_sample_grp.mm <- .rename.intercept(ref_sample_grp.mm, "group")

  stopifnot(diff(ref_sample_grp.mm[[field_names$groupId]]) >= 0)
  group_proxy_positions <- c(TRUE, diff(ref_sample_grp.mm[[field_names$groupId]]) != 0)
  group_sizes <- rle(ref_sample_grp.mm[[field_names$groupId]])$lengths

  ref_sample_grp.agg <- ref_sample_grp.mm[group_proxy_positions, ]

  group_size_rescale <- rep(group_sizes, group_sizes)

  weights_transform <- Matrix::sparseMatrix(
    i=seq_along(group_proxy_positions), j=rep(seq_along(group_sizes), group_sizes),
    x=1 / group_size_rescale)

  prior_weights_agg <- as.vector(prior_weights %*% weights_transform)

  if (!is.null(control.formulae$individual) && nchar(control.formulae$individual) > 0) {
    ref_sample_ind.mm <- as.data.frame(model.matrix(
      as.formula(sprintf("~%s+%s", field_names$groupId, control.formulae$individual)),
      plyr::rename(ref_sample[c(field_names$groupId, names(control.names$individual))], control.names$individual)))
    ref_sample_ind.mm <- .rename.intercept(ref_sample_ind.mm, "individual")
    ref_sample_ind.agg <- aggregate(as.formula(sprintf(".~%s", field_names$groupId)),
                                    ref_sample_ind.mm, FUN=sum)
    ref_sample.agg <- merge(ref_sample_ind.agg, ref_sample_grp.agg,
                            by=field_names$groupId)

    stopifnot(ref_sample.agg[[field_names$groupId]] == ref_sample_grp.agg[[field_names$groupId]])
  } else {
    ref_sample.agg <- ref_sample_grp.agg
  }

  message("Flattening reference sample")
  ref_sample.agg.agg <- aggregate(as.formula(sprintf("%s~.", field_names$groupId)),
                                  ref_sample.agg, FUN=identity)

  group_ids <- ref_sample.agg.agg[, field_names$groupId]
  group_ids <- setNames(group_ids, nm = sprintf("%d.", seq_along(group_ids)))
  group_ids_u <- unlist(group_ids, use.names = TRUE)

  rows_from <- match(group_ids_u, ref_sample.agg[, field_names$groupId])
  rows_to <- trunc(as.numeric(names(group_ids_u)))

  agg_agg_weights_transform <- Matrix::sparseMatrix(
    i=rows_from, j=rows_to, x=1, dimnames=list(ref_sample.agg[, field_names$groupId], NULL))

  weights_transform <- weights_transform %*% agg_agg_weights_transform
  prior_weights_agg_agg <- as.vector(prior_weights_agg %*% agg_agg_weights_transform)

  message("Transposing reference sample")
  ref_sample.agg.agg.m <- t(as.matrix(ref_sample.agg.agg[
    , setdiff(colnames(ref_sample.agg.agg), field_names$groupId)]))

  message("Flattening controls")
  control.totals.list <- plyr::llply(
    control.terms.list,
    function(control.terms) {
      unname(plyr::llply(control.terms, `[[`, 'control'))
    }
  )
  control.totals.dup <- unlist(unname(control.totals.list), use.names=TRUE)

  message("Checking controls for conflicts")
  control.totals.dup.rearrange <- plyr::llply(
    setNames(nm=unique(names(control.totals.dup))),
    function (control.name)
      unname(control.totals.dup[names(control.totals.dup) == control.name])
  )
  control.totals <- sapply(control.totals.dup.rearrange, `[[`, 1L)
  control.totals.conflicts <- sapply(
    control.totals.dup.rearrange,
    function(x) !isTRUE(all.equal(x, rep(x[[1L]], length(x))))
  )
  stopifnot(names(control.totals) == names(control.totals.conflicts))
  if (any(control.totals.conflicts)) {
    warning("  The following controls are conflicting, values will be assumed as follows:\n    ",
            paste(sprintf("%s=%s", names(control.totals)[control.totals.conflicts], control.totals[control.totals.conflicts]),
                  collapse = ", "))
  }

  message("Reordering controls")
  if (!identical(sort(names(control.totals)), sort(rownames(ref_sample.agg.agg.m)))) {
    # Code below depends on halting here!
    stop("  The following controls do not have any corresponding observation in the reference sample:\n    ",
         paste(setdiff(names(control.totals), rownames(ref_sample.agg.agg.m)), collapse=", "), "\n",
         "  The following categories in the reference sample do not have a corresponding control:\n    ",
         paste(setdiff(rownames(ref_sample.agg.agg.m), names(control.totals)), collapse=", "), "\n"
    )
  }

  # The following really assumes that controls are identical!
  control.totals <- control.totals[rownames(ref_sample.agg.agg.m)]

  message("Checking zero-valued controls")
  zero.control.totals <- (control.totals == 0)
  if (any(zero.control.totals)) {
    message("  Found zero-valued controls: ",
            paste(names(control.totals)[zero.control.totals], collapse = ", "))
    zero.observations <- apply(ref_sample.agg.agg.m, 2, function(x) any(x[zero.control.totals] > 0))
    if (any(zero.observations)) {
      zero.observation.weights <- sum(prior_weights_agg_agg[zero.observations])
      warning(
        "  Removing ", sum(zero.observations), " distinct entries from the reference sample ",
        "with a total weight of ", sum(zero.observation.weights))
      prior_weights_agg_agg <- prior_weights_agg_agg[!zero.observations]

      nonzero.observations_w <- which(!zero.observations)

      zero_weights_transform <- Matrix::sparseMatrix(
        i=nonzero.observations_w, j=seq_along(nonzero.observations_w), x=1)
      weights_transform <- weights_transform %*% zero_weights_transform
    } else {
      message("  No observations matching those zero-valued controls.")
    }
    ref_sample.agg.agg.m <- ref_sample.agg.agg.m[!zero.control.totals, !zero.observations]
    control.totals <- control.totals[!zero.control.totals]
  } else
    message("  No zero-valued controls")
  stopifnot(control.totals > 0)

  message("Checking missing observations")
  ref_sample.agg.agg.m.rs <- rowSums(ref_sample.agg.agg.m)
  missing.controls <- (ref_sample.agg.agg.m.rs == 0)
  if (any(missing.controls)) {
    warning(
      "  Found missing observations for the following non-zero controls: ",
      paste(sprintf("%s=%s", names(control.totals)[missing.controls], control.totals[missing.controls]), collapse = ", "))

    control.totals <- control.totals[!missing.controls]
    ref_sample.agg.agg.m <- ref_sample.agg.agg.m[!missing.controls, ]
  }

  message("Computing reverse weights map")
  reverse_weights_transform <- ((1/prior_weights_agg_agg) * Matrix::t(prior_weights * group_size_rescale * weights_transform))
  stopifnot(all.equal(Matrix::diag(reverse_weights_transform %*% weights_transform), rep(1, ncol(weights_transform))))

  message("Normalizing weights")
  prior_weights_agg_agg <- prior_weights_agg_agg / sum(prior_weights_agg_agg) * control.totals[c("(Intercept)_g", "(Intercept)_i")][[1]]

  message("Done!")
  structure(
    list(
      ref_sample=ref_sample.agg.agg.m,
      weights=prior_weights_agg_agg,
      control_totals=control.totals,
      weights_transform=weights_transform,
      reverse_weights_transform=reverse_weights_transform
    ),
    class=c("flat_ml_fit_problem")
  )
}

.control.type.abbrev <- function(control.type) substr(control.type, 1, 1)
.rename.intercept <- function(data, control.type) {
  colnames(data) <- plyr::revalue(colnames(data),
                                  c(`(Intercept)`=sprintf("(Intercept)_%s", .control.type.abbrev(control.type))))
  data
}
