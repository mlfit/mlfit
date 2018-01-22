# nocov start
make_is <- function(class, env = parent.frame()) {
  f <- eval(bquote(function(x) {
    inherits(x, .(class))
  }))
  environment(f) <- env
  f
}

make_new <- function(class, env = parent.frame()) {
  f <- eval(bquote(function(x, ...) {
    structure(x, ..., class = .(class))
  }))
  environment(f) <- env
  f
}

default_print <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
# nocov end
