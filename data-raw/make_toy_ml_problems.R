#' this script converts the toy examples as `fitting_problem` objects
#' to `ml_problem` objects.

toy_example_filedirs <-
  list.files(here::here("inst", "extdata"), full.names = TRUE)

toy_fitting_problems <-
  lapply(toy_example_filedirs, readRDS)

make_ml_problem <- function(x) {
  stopifnot(is(x, "fitting_problem"))
  class(x) <- "ml_problem"
  x
}

toy_ml_problems <-
  lapply(toy_fitting_problems, make_ml_problem)

mapply(function(filedir, toy_example) {
  saveRDS(toy_example, filedir, version = 2)
}, toy_example_filedirs, toy_ml_problems)
