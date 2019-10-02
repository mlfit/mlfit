#' Truncate, Replicate, and Sample
#'
#' For more details about the TRS method see its original paper
#' Lovelace, R. and Ballas, D., 2013. ‘Truncate, replicate, sample’:
#' A method for creating integer weights for spatial microsimulation. Computers,
#' Environment and Urban Systems, 41, pp.1-11. Vancouver
#'
#' @param x a data.frame or ml_fit object
#' @param w a numeric vector which has the same length as the rows in `x`
#' @param hid_col a character indicates household id/group id column
#' @param pid_col a character indicates individual id coluin
#'
#' @return a data.frame
#' @export
#'
#' @examples
#'
#' fp <- readRDS(toy_example("Joint"))
#' fit <- ml_fit_ipu(fp)
#' synthetic_population <- trs(fit)
trs <- function(x, w, hid_col, pid_col, verbose = TRUE) {
  if (is(x, "ml_fit")) {
    return(trs.fitting_problem(x, verbose = verbose))
  }
  return(trs.default(x, w, hid_col, pid_col, verbose = verbose))
}

trs.fitting_problem <- function(x, verbose){
  trs.default(x = x$flat$fitting_problem$refSample,
              w = x$weights,
              hid_col = x$flat$fitting_problem$fieldNames$groupId,
              pid_col = x$flat$fitting_problem$fieldNames$individualId,
              verbose = verbose)
}

trs.default <- function(x, w, hid_col, pid_col, verbose) {
  .patch_verbose()
  message("Checking inputs")
  stopifnot(is.data.frame(x))
  stopifnot(nrow(x) == length(w))
  stopifnot(is.numeric(w))
  stopifnot(!anyNA(w))
  stopifnot(hid_col %in% names(x))
  stopifnot(pid_col %in% names(x))
  if (!is(x, "data.table")) {
    x <- data.table::as.data.table(x)
  } else {
    x <- data.table::copy(x)
  }
  # (T)runcate - seperate conventional weight and probabilistis weight
  message("Truncating")
  pop_sample <- data.table::copy(x)[, `:=`(replica = trunc(w),
                                           prob = w - trunc(w),
                                           weight = w)]

  # (R)elicate - replicate households based on their conventional weights
  message("Replicating")
  pop_R <- pop_sample[rep(1:.N, replica)] %>%
    .[, c("weight", "replica", "prob") := NULL]

  # (S)ample - sample households based on their probabilistic weights
  n_total <-
    sum(pop_sample[pop_sample[, .I[1], by = c(hid_col)]$V1][["weight"]])
  n_replica <-
    sum(pop_sample[pop_sample[, .I[1], by = c(hid_col)]$V1][["replica"]])
  n_sample <- n_total - n_replica

  if (n_sample > 0) {
    message("Sampling")
    hh_sample <-
      pop_sample[pop_sample[, .I[1], by = c(hid_col)]$V1]

    # random draw
    sampled_hh <-
      sample(
        size = n_sample,
        x = hh_sample[[hid_col]],
        prob = hh_sample[["prob"]],
        replace = FALSE
      )

    # turn it into a data.table
    sampled_hh <-
      data.table::data.table(id = sampled_hh) %>%
      data.table::setnames(., old = "id", new = hid_col)

    hh_sample <-
      pop_sample[pop_sample[, .I[1], by = c(hid_col)]$V1]

    sampled_hh <-
      sample(
        size = n_sample,
        x = hh_sample[[hid_col]],
        prob = hh_sample[["prob"]],
        replace = FALSE
      )

    # turn it into a data.table
    sampled_hh <-
      data.table::data.table(id = sampled_hh) %>%
      data.table::setnames(., old = "id", new = hid_col)

    # merge individuals to sampled households
    pop_S <-
      sampled_hh[x, , on = c(hid_col), nomatch = 0]

    # make records from sampling and replication have the same column order
    data.table::setcolorder(pop_S, names(pop_R))

    # return
    return(rbind(pop_R, pop_S))

  } else {
    message("The sum of the remaining weights after the truncation equal to 0. No sampling is needed.")
    return(pop_R)
  }


}


#' assign id
#'
#' @param x data.frame
#' @param hid_col character
#' @param pid_col character
#'
#' @return data.frame
#' @export
assign_ids <- function(x, hid_col, pid_col, keep_id_cols = FALSE, verbose = TRUE) {
  .patch_verbose()
  message("Checking inputs")
  stopifnot(is.data.frame(x))
  stopifnot(hid_col %in% names(x))
  stopifnot(pid_col %in% names(x))
  if (!is(x, "data.table")) {
    x <- data.table::as.data.table(x)
  }

  if (hid_col == "hid") {
    setnames(x, old = "hid", new = "hid_old")
    hid_col <- "hid_old"
  }

  if (pid_col == "pid") {
    setnames(x, old = "pid", new = "pid_old")
    pid_col <- "pid_old"
  }

  id_cols <- c(hid_col, pid_col)

  message("Assigning id cols")
  x_new <-
    data.table::copy(x) %>%
    .[, within_hh_pid := 1:.N, by = c(hid_col, pid_col)] %>%
    .[, hid := .GRP, by = c(hid_col, "within_hh_pid")] %>%
    .[, pid := 1:.N] %>%
    data.table::setcolorder(., c("pid", "hid"))

  if (keep_id_cols) {
    message("Returning new data with original id cols")
    return(x_new)
  }

  message("Returning new data without original id cols")
  x_new %>%
    .[, c(id_cols) := NULL]

  x_new
}


