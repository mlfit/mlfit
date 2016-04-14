`%+%` <- function(x, y) paste0(x, y)

remove_leading_zeros <- function(x) {
  x <- c(0, x)
  x[-seq_len(rle(x)$lengths[[1]])]
}
