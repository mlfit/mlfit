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


