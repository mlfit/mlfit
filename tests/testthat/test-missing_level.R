test_that("A level is missing from the reference sample should raise a warning", {
  ref_sample <-
    data.frame(
      pid = c(1, 2, 3, 4, 5),
      hid = c(1, 1, 2, 2, 3),
      gender = c("male", "female", "male", "male", "female"),
      marital_status = c("married", "single", "married", "married", "married"),
      hhsize = c(2, 2, 2, 2, 1)
    )

  p_control <-
    data.frame(
      gender = rep(c("male", "female"), 2),
      marital_status = c(rep("married", 2), rep("single", 2)),
      count = 1:4
    )

  h_control <-
    data.frame(
      hhsize = c(1, 2),
      count = c(2, 1)
    )

  toy_problem <-
    fitting_problem(
      ref_sample = ref_sample,
      controls = list(
        individual = list(p_control),
        group = list(h_control)
      ),
      field_names = special_field_names("hid", "pid", count = "count")
    )

  expect_warning(
    ml_fit(algorithm = "ipu", toy_problem),
    regexp = "Found missing observations for the following non-zero controls: gender_i_male:marital_status_i_single"
  )
})
