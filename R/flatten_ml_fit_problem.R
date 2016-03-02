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
#' @importFrom kimisc coalesce.na
#' @importFrom plyr laply adply
#' @export
#' @examples
#' path <- system.file("extdata/minitoy", package="MultiLevelIPF")
#' flatten_ml_fit_problem(fitting_problem = import_IPAF_results(path))
flatten_ml_fit_problem <- function(fitting_problem, verbose = FALSE) {
  .check_is_fitting_problem(fitting_problem)
  ref_sample <- fitting_problem$refSample
  controls <- fitting_problem$controls
  field_names <- fitting_problem$fieldNames
  prior_weights <- fitting_problem$priorWeights

  .patch_verbose()

  if (is.null(prior_weights)) {
    # If not given, assume uniform prior weights
    prior_weights <- rep(1, nrow(ref_sample))
  }

  message("Collecting controls")
  control.names.list <- llply(
    controls,
    function(control.list) {
      control.columns <- llply(
        control.list,
        function(control) {
          # Secure against data.table
          control <- as.data.frame(control)
          count_name <- get_count_field_name(control, field_names$count, message)
          setdiff(colnames(control), count_name)
        }
      )
    }
  )

  control_names <- unique(unlist(control.names.list, recursive = TRUE))

  if (!all(control_names %in% colnames(ref_sample))) {
    stop("Control variable(s) not found: ",
         paste0(setdiff(control_names, colnames(ref_sample)), collapse = ", "))
  }

  message("Converting to factor")
  ref_sample[control_names] <-
    lapply(ref_sample[, control_names, drop = FALSE], as.factor)

  has_na <- vapply(ref_sample[control_names], anyNA, logical(1L))
  if (any(has_na)) {
    stop("NA values for control variables in reference sample: ",
         paste0(control_names[has_na], collapse = ", "))
  }

  message("Preparing controls")
  control.terms.list <- llply(
    setNames(nm=names(controls)),
    function(control.type) {
      control.list <- controls[[control.type]]
      control.columns <- llply(
        control.list,
        control.type = control.type,
        function(control, control.type) {
          # Secure against data.table
          control <- as.data.frame(control)
          count_name <- get_count_field_name(control, field_names$count, message)

          control.and.count.names <- setNames(nm=colnames(control))
          control.names.unordered <- setdiff(control.and.count.names, count_name)
          control.names <- colnames(ref_sample)[colnames(ref_sample) %in% control.names.unordered]
          stopifnot(length(control.names) == length(control.names.unordered))

          control[control.names] <- lapply(
            control[, control.names, drop = FALSE],
            as.factor
          )

          control_levels <- lapply(control[control.names], levels)
          ref_sample_levels <- lapply(ref_sample[control.names], levels)
          if (!identical(control_levels, ref_sample_levels)) {
            levels_identical <-
              mapply(identical, control_levels, ref_sample_levels)
            stop(
              "Factor level mismatch between control and reference sample:\n",
              paste0(
                "- ", control.names[!levels_identical], " (",
                vapply(control_levels[!levels_identical],
                       paste, collapse = ", ",
                       character(1L)),
                " vs. ",
                vapply(ref_sample_levels[!levels_identical],
                       paste, collapse = ", ",
                       character(1L)),
                ")",
                collapse = "\n")
            )
          }

          # Avoids error: "contrasts can be applied only to factors with 2 or more levels"
          control.levels <- vapply(
            control[control.names],
            function(f) {
              length(levels(f))
            },
            integer(1))
          if (any(control.levels == 0)) {
            stop("All control variables must be factors or characters.
                 Offending control variable(s): ",
                 paste0(control.names[control.levels == 0], collapse = ", "))
          }
          control.names <- control.names[control.levels > 1]

          new.control.names <- sprintf("%s_%s_", control.names, .control.type.abbrev(control.type))
          control.and.count.names[control.names] <- new.control.names
          colnames(control) <- control.and.count.names

          control.term <- paste0(new.control.names, collapse="*")
          if (nchar(control.term) == 0)
            control.term <- "1"

          control.mm <- model.matrix(
            as.formula(sprintf("~%s", control.term)), # nolint
            control)
          control.mm <- .rename.intercept(control.mm, control.type)

          list(
            control.names=control.names,
            new.control.names=new.control.names,
            term=control.term,
            control = (control[[count_name]] %*% control.mm)[1,, drop = TRUE]
          )
        }
      )
    }
  )

  control.formulae <- llply(
    control.terms.list,
    function(control.terms) {
      paste(laply(control.terms, `[[`, "term"), collapse="+")
    }
  )

  # List of "individual" and "group"
  #   Each item contains a named vector: names are column names in the original
  #   dataset, values are new names in the mangled dataset
  control.names <- llply(
    control.terms.list,
    function(control.terms) {
      control.names <- unlist(llply(unname(control.terms), function(control.term)
        setNames(control.term$new.control.names, control.term$control.names)))
      control.names[!duplicated(control.names)]
    }
  )

  if (is.null(control.names$group)) {
    stop("Need at least one control at group level.", call. = FALSE)
  }

  message("Preparing reference sample (groups)")
  if (!(field_names$groupId %in% colnames(ref_sample)))
    stop("Group ID column ", field_names$groupId, " not found in reference sample.")
  stopifnot(is.numeric(ref_sample[[field_names$groupId]]))
  formula_grp <- sprintf("~%s", field_names$groupId) # nolint
  if (nchar(control.formulae$group) > 0) {
    formula_grp <- sprintf("%s+%s", formula_grp, control.formulae$group)
  }
  ref_sample_grp.mm <- as.data.frame(model.matrix(as.formula(formula_grp),
    plyr::rename(ref_sample[c(field_names$groupId, names(control.names$group))], control.names$group)))
  ref_sample_grp.mm <- .rename.intercept(ref_sample_grp.mm, "group")

  message("Splitting")
  if (any(diff(ref_sample_grp.mm[[field_names$groupId]]) < 0)) {
    stop("Reference sample not sorted by group ID column ", field_names$groupId, ".")
  }
  group_proxy_positions <- c(TRUE, diff(ref_sample_grp.mm[[field_names$groupId]]) != 0)
  group_sizes <- rle(ref_sample_grp.mm[[field_names$groupId]])$lengths

  ref_sample_grp.agg <- ref_sample_grp.mm[group_proxy_positions,, drop = FALSE]

  message("Transforming weights")
  group_size_rescale <- rep(group_sizes, group_sizes)

  weights_transform <- Matrix::sparseMatrix(
    i=seq_along(group_proxy_positions), j=rep(seq_along(group_sizes), group_sizes),
    x=1 / group_size_rescale)

  prior_weights_agg <- as.vector(prior_weights %*% weights_transform)

  if (!is.null(control.formulae$individual) && nchar(control.formulae$individual) > 0) {
    message("Preparing reference sample (individuals)")
    ref_sample_ind.mm <- as.data.frame(model.matrix(
      as.formula(sprintf("~%s+%s", field_names$groupId, control.formulae$individual)), # nolint
      plyr::rename(ref_sample[c(field_names$groupId, names(control.names$individual))], control.names$individual)))

    message("Aggregating")
    ref_sample_ind.mm <- .rename.intercept(ref_sample_ind.mm, "individual")
    ref_sample_ind.agg <- aggregate(as.formula(sprintf(".~%s", field_names$groupId)),
                                    ref_sample_ind.mm, FUN=sum)

    message("Merging")
    ref_sample.agg <- merge(ref_sample_ind.agg, ref_sample_grp.agg,
                            by=field_names$groupId)

    stopifnot(ref_sample.agg[[field_names$groupId]] == ref_sample_grp.agg[[field_names$groupId]])
  } else {
    ref_sample.agg <- ref_sample_grp.agg
  }

  message("Flattening reference sample")
  ref_sample.agg.agg <- aggregate(as.formula(sprintf("%s~.", field_names$groupId)),
                                  ref_sample.agg, FUN=identity, simplify = FALSE)

  group_ids <- ref_sample.agg.agg[, field_names$groupId, drop = TRUE]
  group_ids <- setNames(group_ids, nm = sprintf("%d.", seq_along(group_ids)))
  group_ids_u <- unlist(group_ids, use.names = TRUE)

  rows_from <- match(group_ids_u, ref_sample.agg[, field_names$groupId, drop = TRUE])
  rows_to <- trunc(as.numeric(names(group_ids_u)))

  agg_agg_weights_transform <- Matrix::sparseMatrix(
    i=rows_from, j=rows_to, x=1, dimnames=list(ref_sample.agg[, field_names$groupId, drop = TRUE], NULL))

  weights_transform <- weights_transform %*% agg_agg_weights_transform
  prior_weights_agg_agg <- as.vector(prior_weights_agg %*% agg_agg_weights_transform)

  message("Transposing reference sample")
  ref_sample.agg.agg.m <- t(as.matrix(ref_sample.agg.agg[
    , setdiff(colnames(ref_sample.agg.agg), field_names$groupId), drop = FALSE]))

  message("Flattening controls")
  control.totals.list <- llply(
    control.terms.list,
    function(control.terms) {
      unname(llply(control.terms, `[[`, "control"))
    }
  )
  control.totals.dup <- unlist(unname(control.totals.list), use.names=TRUE)

  message("Checking controls for conflicts")
  control.totals.dup.rearrange <- llply(
    setNames(nm=unique(names(control.totals.dup))),
    function (control.name)
      unname(control.totals.dup[names(control.totals.dup) == control.name])
  )

  control.totals <- sapply(control.totals.dup.rearrange, `[[`, 1L)
  if (length(control.totals) == 0L)
    control.totals <- numeric()

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
  intersect_names <- intersect(sort(rownames(ref_sample.agg.agg.m)), names(control.totals))

  if (length(control.totals) > length(intersect_names)) {
    warning(
      "  The following controls do not have any corresponding observation in the reference sample:\n    ",
      paste(setdiff(names(control.totals), intersect_names), collapse=", "))
  }

  if (nrow(ref_sample.agg.agg.m) > length(intersect_names)) {
    warning(
      "  The following categories in the reference sample do not have a corresponding control:\n    ",
      paste(setdiff(rownames(ref_sample.agg.agg.m), intersect_names), collapse=", "))
  }

  ref_sample.agg.agg.m <- ref_sample.agg.agg.m[intersect_names,, drop = FALSE]
  control.totals <- control.totals[intersect_names]

  message("Checking zero-valued controls")
  zero.control.totals <- (control.totals == 0)
  if (any(zero.control.totals)) {
    message("  Found zero-valued controls (showing the first 10): ",
            paste(head(names(control.totals)[zero.control.totals], 10), collapse = ", "))
    zero.observations <- apply(ref_sample.agg.agg.m, 2, function(x) any(x[zero.control.totals] > 0))
    if (any(zero.observations)) {
      zero.observation.weights <- sum(prior_weights_agg_agg[zero.observations])
      warning(
        "  Removing ", sum(zero.observations), " distinct entries from the reference sample ",
        "(corresponding to zero-valued controls) with a total weight of ", sum(zero.observation.weights))
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
  reverse_weights_transform <- ( (1 / prior_weights_agg_agg) * Matrix::t(prior_weights * group_size_rescale * weights_transform))
  stopifnot(all.equal(Matrix::diag(reverse_weights_transform %*% weights_transform), rep(1, ncol(weights_transform))))

  message("Normalizing weights")
  prior_weights_agg_agg <- prior_weights_agg_agg / sum(prior_weights_agg_agg) *
    unname(coalesce.na(control.totals["(Intercept)_g"],
                       control.totals["(Intercept)_i"],
                       sum(prior_weights_agg_agg)))

  message("Done!")
  structure(
    list(
      ref_sample=ref_sample.agg.agg.m,
      weights=prior_weights_agg_agg,
      control_totals=control.totals,
      weights_transform=weights_transform,
      reverse_weights_transform=reverse_weights_transform,
      fitting_problem=fitting_problem
    ),
    class=c("flat_ml_fit_problem")
  )
}

.control.type.abbrev <- function(control.type) {
  substr(control.type, 1, 1)
}

#' @importFrom plyr revalue
.rename.intercept <- function(data, control.type) {
  colnames(data) <- revalue(
    colnames(data),
    c(`(Intercept)`=sprintf("(Intercept)_%s", .control.type.abbrev(control.type))))
  data
}

get_count_field_name <- function(control, name, message) {
  if (is.null(name)) {
    classes <- vapply(control, function(x) class(x)[[1L]], character(1))
    numerics <- which(classes %in% c("integer", "numeric"))

    if (length(numerics) == 0) {
      stop("No numeric column found among control columns ",
           paste(names(control), collapse = ", "), ".")
    }

    if (length(numerics) > 1) {
      numerics <- numerics[[1L]]
    }

    message("Using ", names(control)[numerics],
            " as count column for ",
            paste(names(control)[-numerics], collapse = ", "), ".")
    name <- names(control)[numerics]
  }
  name
}

expand_weights <- function(flat_weights, flat) {
  requireNamespace("Matrix")
  unname(as.vector(flat_weights %*% flat$reverse_weights_transform))
}
