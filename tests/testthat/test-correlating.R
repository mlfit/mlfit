test_that("trivially correlated variables", {
  N <- 1000
  M <- 60
  x <- runif(N) * M

  d <- rep(runif(1), N)

  rs <-
    data.frame(
      x2 = cut(x, seq(0, M, by = 2), right = FALSE),
      x5 = cut(x, seq(0, M, by = 5), right = FALSE),
      x10 = cut(x, seq(0, M, by = 10), right = FALSE)
    )
  rs$id <- seq_along(rs$x2)

  x2c <- data.frame(x2 = forcats::fct_inorder(levels(rs$x2)))
  x2c$N <- 2

  x5c <- data.frame(x5 = forcats::fct_inorder(factor(levels(rs$x5))))
  x5c$N <- 5

  x10c <- data.frame(x10 = forcats::fct_inorder(factor(levels(rs$x10))))
  x10c$N <- 10

  problem <- ml_problem(
    rs,
    field_names = special_field_names("id", "id", count = "N"),
    individual_controls = list(x2c, x5c, x10c),
    group_controls = list()
  )

  flat <- mlfit::flatten_ml_fit_problem(
    problem,
    model_matrix_type = "combined", verbose = TRUE
  )

  fit <- ml_fit_dss(flat, method = "linear")

  expect_equal(unname(fit$residuals), rep(0, length(fit$residuals)))
})
