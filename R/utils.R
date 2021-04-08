`%+%` <- function(x, y) paste0(x, y)

remove_leading_zeros <- function(x) {
  x <- c(0, x)
  x[-seq_len(rle(x)$lengths[[1]])]
}

.check_is_integer <- function(x) {
  if (isTRUE(all.equal(x, as.integer(x)))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
