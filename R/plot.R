#' @export
#' @importFrom ggplot2 autoplot
autoplot.ml_fit <- function(object, metric = c("rel_residual", "residual"), ...) {
  metric <- match.arg(metric)
  plot_data <-
    combine_margins(object) %>%
    tidy_combined_margins()

  if (metric == "rel_residual") {
    plot_data$..rel_residual.. <- plot_data$..rel_residual.. * 100
  }

  y_label <- switch(metric,
    "rel_residual" = "Relative Residual (%)",
    "residual" = "Residual"
  )

  ggplot(
    data = plot_data,
    mapping = aes(
      y = .data[["..cat.."]],
      x = round(.data[[paste0("..", metric, "..")]] , 2)
    )
  ) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    geom_boxplot() +
    labs(x = "control", y = y_label) +
    facet_wrap(~ .data[["..level.."]] + .data[["..var.."]], scales = "free") +
    theme_bw()
}

#' @export
#' @importFrom graphics plot
plot.ml_fit <- function(x, ...) {
  print(autoplot(x, ...))
}