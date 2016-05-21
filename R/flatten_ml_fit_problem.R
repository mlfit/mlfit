#' Return a flattened representation of a multi-level fitting problem instance
#'
#' This function transforms a multi-level fitting problem to a representation
#' more suitable for applying the algorithms:  A matrix with one row per controlled
#' attribute and one column per household, a weight vector with one weight
#' per household, and a control vector.
#'
#' @details
#' The standard way to build a model matrix (\code{model_matrix = "combined"})
#' is to include intercepts and avoid repeating redundant attributes.
#' A simpler model matrix specification, available via \code{model_matrix = "separate"},
#' is used by Ye et al. (2009):
#' Here, simply one column per target value is used, which
#' results in a larger model matrix if more than one control is given.
#'
#' @inheritParams ml_fit
#' @param model_matrix_type Which model matrix building strategy to use? See details.
#' @return An object of classes \code{flat_ml_fit_problem},
#'   essentially a named list.
#' @seealso \code{\link{ml_fit}}
#' @importFrom kimisc coalesce.na
#' @importFrom plyr laply adply
#' @export
#' @examples
#' path <- toy_example("Tiny")
#' flatten_ml_fit_problem(fitting_problem = readRDS(path))
flatten_ml_fit_problem <- function(fitting_problem,
                                   model_matrix_type = c("combined", "separate"),
                                   verbose = FALSE) {
  .check_is_fitting_problem(fitting_problem)
  field_names <- fitting_problem$fieldNames
  prior_weights <- fitting_problem$priorWeights

  model_matrix_type <- match.arg(model_matrix_type)
  model_matrix <- .get_model_matrix_fun(model_matrix_type)

  .patch_verbose()

  prepared_ref_sample <- .prepare_ref_sample_and_controls(fitting_problem, verbose = verbose)
  ref_sample <- prepared_ref_sample$ref_sample
  controls <- prepared_ref_sample$controls
  control_names <- prepared_ref_sample$control_names

  control.terms.list <- .get_control_terms_list(controls, model_matrix, verbose)

  control_formula_components <- lapply(
    control.terms.list,
    function(control.term) {
      formula_components <- vapply(control.term, `[[`, character(1L), "term")
      unique(formula_components)
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

  if (length(control_formula_components$group) > 0L) {
    message("Preparing reference sample (groups)")
    formula_grp <- c(field_names$groupId, control_formula_components$group)
    ref_sample_grp.mm <- model_matrix(formula_grp,
      plyr::rename(ref_sample[c(field_names$groupId, names(control.names$group))], control.names$group),
      "group") %>%
      as.data.frame
  } else {
    ref_sample_grp.mm <- ref_sample[, field_names$groupId, drop = FALSE]
  }

  message("Splitting")
  gid_lookup <-
    data_frame(gid = ref_sample[[field_names$groupId]]) %>%
    mutate_(iidx = ~seq_along(gid)) %>%
    mutate_(canonical = ~match(gid, gid)) %>%
    mutate_(proxy = ~!duplicated(canonical)) %>%
    mutate_(gidx = ~cumsum(proxy)[canonical]) %>%
    select_(~-canonical) %>%
    group_by_(~gid) %>%
    mutate_(n = ~length(gid)) %>%
    ungroup

  ref_sample_grp.agg <- ref_sample_grp.mm[gid_lookup$proxy,, drop = FALSE]

  weights_transform <- Matrix::sparseMatrix(
    i = gid_lookup$iidx,
    j = gid_lookup$gidx,
    x = 1 / gid_lookup$n)

  message("Transforming weights")
  if (is.null(prior_weights)) {
    # If not given, assume uniform prior weights
    prior_weights <- rep(1, nrow(ref_sample))
  }
  prior_weights_agg <- as.vector(prior_weights %*% weights_transform)

  if (length(control_formula_components$individual) > 0) {
    message("Preparing reference sample (individuals)")
    formula_ind <- c(field_names$groupId, control_formula_components$individual)
    ref_sample_ind.mm <- model_matrix(formula_ind,
      plyr::rename(ref_sample[c(field_names$groupId, names(control.names$individual))], control.names$individual),
      "individual") %>%
      as.data.frame

    message("Aggregating")
    ref_sample_ind.mm <- .rename.intercept(ref_sample_ind.mm, "individual")
    ref_sample_ind.agg <-
      ref_sample_ind.mm %>%
      group_by_(field_names$groupId) %>%
      summarize_each_(funs(sum), as_names(setdiff(colnames(ref_sample_ind.mm), field_names$groupId))) %>%
      ungroup

    message("Merging")
    ref_sample.agg <- merge(ref_sample_ind.agg, ref_sample_grp.agg,
                            by=field_names$groupId)

    stopifnot(ref_sample.agg[[field_names$groupId]] == ref_sample_grp.agg[[field_names$groupId]])
  } else {
    ref_sample.agg <- ref_sample_grp.agg
  }

  control.totals <- .flatten_controls(control.terms.list = control.terms.list,
                                      verbose = verbose)

  message("Converting reference sample to matrix")
  ref_sample.agg.m <- as.matrix(ref_sample.agg[
    , setdiff(colnames(ref_sample.agg), field_names$groupId), drop = FALSE])

  message("Reordering controls")
  intersect_names <- intersect(sort(colnames(ref_sample.agg.m)), names(control.totals))

  if (length(control.totals) > length(intersect_names)) {
    warning(
      "  The following controls do not have any corresponding observation in the reference sample:\n    ",
      paste(setdiff(names(control.totals), intersect_names), collapse=", "))
  }

  if (ncol(ref_sample.agg.m) > length(intersect_names)) {
    warning(
      "  The following categories in the reference sample do not have a corresponding control:\n    ",
      paste(setdiff(colnames(ref_sample.agg.m), intersect_names), collapse=", "))
  }

  ref_sample.agg.m <- ref_sample.agg.m[, intersect_names, drop = FALSE]
  control.totals <- control.totals[intersect_names]

  message("Checking zero-valued controls")
  zero.control.totals <- (control.totals == 0)
  if (any(zero.control.totals)) {
    message("  Found zero-valued controls (showing the first 10): ",
            paste(head(names(control.totals)[zero.control.totals], 10), collapse = ", "))
    zero.observations <- apply(ref_sample.agg.m, 1, function(x) any(x[zero.control.totals] > 0))
    if (any(zero.observations)) {
      zero.observation.weights <- sum(prior_weights_agg[zero.observations])
      warning(
        "  Removing ", sum(zero.observations), " distinct entries from the reference sample ",
        "(corresponding to zero-valued controls) with a total weight of ", sum(zero.observation.weights))
      prior_weights_agg <- prior_weights_agg[!zero.observations]

      nonzero.observations_w <- which(!zero.observations)

      zero_weights_transform <- Matrix::sparseMatrix(
        i=nonzero.observations_w, j=seq_along(nonzero.observations_w), x=1)
      weights_transform <- weights_transform %*% zero_weights_transform
    } else {
      message("  No observations matching those zero-valued controls.")
    }
    ref_sample.agg.m <- ref_sample.agg.m[!zero.observations, !zero.control.totals]
    control.totals <- control.totals[!zero.control.totals]
  } else
    message("  No zero-valued controls")
  stopifnot(control.totals > 0)

  message("Checking missing observations")
  ref_sample.agg.m.rs <- colSums(ref_sample.agg.m)
  missing.controls <- (ref_sample.agg.m.rs == 0)
  if (any(missing.controls)) {
    warning(
      "  Found missing observations for the following non-zero controls: ",
      paste(sprintf("%s=%s", names(control.totals)[missing.controls], control.totals[missing.controls]), collapse = ", "))

    control.totals <- control.totals[!missing.controls]
    ref_sample.agg.m <- ref_sample.agg.m[, !missing.controls]
  }

  message("Computing reverse weights map")
  reverse_weights_transform <- ( (1 / prior_weights_agg) * Matrix::t(prior_weights * gid_lookup$n * weights_transform))
  stopifnot(all.equal(Matrix::diag(reverse_weights_transform %*% weights_transform), rep(1, ncol(weights_transform))))

  message("Normalizing weights")
  prior_weights_agg <- prior_weights_agg / sum(prior_weights_agg) *
    unname(coalesce.na(control.totals["(Intercept)_g"],
                       control.totals["(Intercept)_i"],
                       sum(prior_weights_agg)))

  message("Done!")
  new_flat_ml_fit_problem(
    list(
      ref_sample = ref_sample.agg.m,
      weights = prior_weights_agg,
      target_values = control.totals,
      weights_transform = weights_transform,
      reverse_weights_transform = reverse_weights_transform,
      model_matrix_type = model_matrix_type,
      fitting_problem = fitting_problem
    )
  )
}




# Prepare ref sample and controls -----------------------------------------

.prepare_ref_sample_and_controls <- function(fitting_problem, verbose) {
  .patch_verbose()

  ref_sample <- fitting_problem$refSample
  controls <- fitting_problem$controls
  field_names <- fitting_problem$fieldNames

  if (length(controls$individual) + length(controls$group) == 0L) {
    stop("Need at least one control at individual or group level.",
         call. = FALSE)
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

  message("Checking controls")
  prepared_controls <- llply(
    setNames(nm=names(controls)),
    function(control.type) {
      control.list <- controls[[control.type]]
      control.columns <- llply(
        control.list,
        control.type = control.type,
        function(control, control.type) {
          # Secure against data.table
          control <- as.data.frame(control)
          control.names <- .ordered_control_names(ref_sample, control, field_names)

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
            stop("All control variables must be factors or characters. ",
                 "Offending control variable(s): ",
                 paste0(control.names[control.levels == 0], collapse = ", "))
          }

          # Avoids hard-to-understand errors if categories are NA
          control.category.na <- vapply(
            control[control.names],
            function(f) any(is.na(f)),
            logical(1))
          if (any(control.category.na)) {
            stop("NA values in control variables not supported. ",
                 "Offending control variable(s): ",
                 paste0(control.names[control.category.na], collapse = ", "))
          }

          # Make sure count column is at position 1
          count_name <- get_count_field_name(control, field_names$count, message)
          control[c(count_name, control.names)]
        }
      )
    }
  )

  message("Checking group ID column")
  if (!(field_names$groupId %in% colnames(ref_sample)))
    stop("Group ID column ", field_names$groupId, " not found in reference sample.")

  list(
    ref_sample = ref_sample,
    controls = prepared_controls,
    control_names = control_names
  )
}

.ordered_control_names <- function(ref_sample, control, field_names) {
  count_name <- get_count_field_name(control, field_names$count, message)
  control.and.count.names <- setNames(nm=colnames(control))
  control.names.unordered <- setdiff(control.and.count.names, count_name)
  control.names <- colnames(ref_sample)[colnames(ref_sample) %in% control.names.unordered]
  stopifnot(length(control.names) == length(control.names.unordered))
  control.names
}

.updated_control_colnames <- function(control, control_names, new_control_names) {
  control_and_count_names <- setNames(nm=colnames(control))
  control_and_count_names[control_names] <- new_control_names
  control_and_count_names
}

# Control terms -----------------------------------------------------------

.get_control_terms_list <- function(controls, model_matrix, verbose) {
  .patch_verbose()

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

          control.names <- colnames(control)[-1]
          count_name <- colnames(control)[[1]]

          # Avoids error: "contrasts can be applied only to factors with 2 or more levels"
          control.levels <- vapply(
            control[control.names],
            function(f) {
              length(levels(f))
            },
            integer(1))
          control.names <- control.names[control.levels > 1]

          new.control.names <- sprintf("%s_%s_", control.names, .control.type.abbrev(control.type))
          colnames(control) <- .updated_control_colnames(control, control.names, new.control.names)

          control.term <- paste0(new.control.names, collapse="*")
          if (nchar(control.term) == 0)
            control.term <- "1"

          control.mm <- model_matrix(control.term, control, control.type)

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
}


# Model matrix ------------------------------------------------------------

.model_matrix_combined <- function(formula_components, data, control.type) {
  formula_as_character <- paste0("~", paste(formula_components, collapse = "+"))
  mm <- stats::model.matrix(as.formula(formula_as_character), data)
  .rename.intercept(mm, control.type)
}

.model_matrix_separate <- function(formula_components, data, control.type) {
  matrices <- lapply(formula_components, .model_matrix_one, data, control.type)

  if (any(duplicated(sapply(matrices, colnames)))) browser()

  do.call(cbind, matrices)
}

.model_matrix_one <- function(formula_component, data, control.type) {
  col_names <- strsplit(formula_component, "[:*]")[[1L]]
  if (length(col_names) <= 1L) {
    if (formula_component == "1")
      formula_as_character <- "~1"
    else
      formula_as_character <- paste0("~", formula_component, "-1")

    mm <- stats::model.matrix(as.formula(formula_as_character), data)
    .rename.intercept(mm, control.type)
  } else {
    col_levels <- Map(function(name, value)
      kimisc::ofactor(paste0(name, levels(value))),
      col_names, data[col_names])
    grid <- do.call(expand.grid, col_levels)
    all_levels <- .combine_levels(grid)

    col_values <- as.data.frame(Map(
      function(x, new_levels) `levels<-`(x, new_levels),
      data[col_names],
      col_levels))
    all_values <- factor(.combine_levels(col_values), levels = all_levels)

    wide <- Matrix::sparseMatrix(seq_len(nrow(data)), as.integer(all_values), x = 1)
    colnames(wide) <- all_levels
    as.matrix(wide)
  }
}

.combine_levels <- function(x) {
  do.call(paste, c(x, list(sep = ":")))
}

.get_model_matrix_fun <- function(model_matrix) {
  switch(
    model_matrix,
    combined = .model_matrix_combined,
    separate = .model_matrix_separate,
    stop("Unknown model matrix function: ", model_matrix, call. = FALSE)
  )
}

.rename.intercept <- function(data, control.type) {
  new_intercept_name <- paste0("(Intercept)_", .control.type.abbrev(control.type))
  colnames(data)[colnames(data) == "(Intercept)"] <- new_intercept_name
  data
}

.control.type.abbrev <- function(control.type) {
  substr(control.type, 1, 1)
}


# Flattening controls -----------------------------------------------------

.flatten_controls <- function(control.terms.list, verbose) {
  .patch_verbose()

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

  control.totals
}


# Utils -------------------------------------------------------------------

as_names <- function(x) {
  lapply(x, as.name)
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


# S3 ----------------------------------------------------------------------

new_flat_ml_fit_problem <- make_new("flat_ml_fit_problem")

#' @export
#' @rdname flatten_ml_fit_problem
#' @param x An object
as.flat_ml_fit_problem <- function(x, model_matrix_type = c("combined", "separate"), ...)
  UseMethod("as.flat_ml_fit_problem", x)

#' @export
as.flat_ml_fit_problem.flat_ml_fit_problem <- function(x, model_matrix_type = c("combined", "separate"), ...) {
  model_matrix_type <- match.arg(model_matrix_type, several.ok = TRUE)
  if (!(x$model_matrix_type %in% model_matrix_type)) {
    stop("Need flat problem with model matrix type ", paste(model_matrix_type, collapse = ", "),
         ", got ", x$model_matrix_type, ".", call. = FALSE)
  }
  x
}

#' @export
as.flat_ml_fit_problem.fitting_problem <- function(x, model_matrix_type = c("combined", "separate"), verbose = FALSE, ...) {
  model_matrix_type <- match.arg(model_matrix_type, several.ok = TRUE)[[1L]]
  flatten_ml_fit_problem(x, model_matrix_type = model_matrix_type, verbose = verbose)
}

#' @export
format.flat_ml_fit_problem <- function(x, ...) {
  c(
    "An object of class flat_ml_fit_problem",
    "  Dimensions: " %+% ncol(x$ref_sample) %+% " groups, " %+%
      nrow(x$ref_sample) %+% " target values",
    "  Model matrix type: " %+% x$model_matrix_type,
    "  Original fitting problem:",
    "  " %+% format(x$fitting_problem)
  )
}

#' @export
print.flat_ml_fit_problem <- default_print
