make_ml_list <- function(x, ml_class) {
  fun.name <- sprintf("is_%s", ml_class)
  if (!exists(fun.name)) {
    stop("Unknown ml_class: ", ml_class)
  }
  is_ml_class <- get(fun.name)
  if (is_ml_class(x)) {
    x <- list(x)
  }
  if (!all(sapply(x, is_ml_class))) {
    stop(sprintf("Not all objects in the list are `%s`.", ml_class))
  }
  x
}