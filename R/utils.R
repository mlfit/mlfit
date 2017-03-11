`%+%` <- function(x, y) paste0(x, y)

remove_leading_zeros <- function(x) {
  x <- c(0, x)
  x[-seq_len(rle(x)$lengths[[1]])]
}

coalesce_null <- function(first, ...) {
  if (!is.null(first) || length(list(...)) == 0L)
    first
  else
    coalesce_null(...)
}
