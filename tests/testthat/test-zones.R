ref_sample <- tibble::tribble(
  ~HHNR, ~PNR, ~APER, ~HH_VAR, ~P_VAR, ~REGION,
  1, 1, 3, 1, 1, 1,
  1, 2, 3, 1, 2, 1,
  1, 3, 3, 1, 3, 1,
  2, 4, 2, 1, 1, 1,
  2, 5, 2, 1, 3, 1,
  3, 6, 3, 1, 1, 1,
  3, 7, 3, 1, 1, 1,
  3, 8, 3, 1, 2, 1,
  4, 9, 3, 2, 1, 1,
  4, 10, 3, 2, 3, 1,
  4, 11, 3, 2, 3, 1,
  5, 12, 3, 2, 2, 1,
  5, 13, 3, 2, 2, 1,
  5, 14, 3, 2, 3, 1,
  6, 15, 2, 2, 1, 1,
  6, 16, 2, 2, 2, 1,
  7, 17, 5, 2, 1, 1,
  7, 18, 5, 2, 1, 1,
  7, 19, 5, 2, 2, 1,
  7, 20, 5, 2, 3, 1,
  7, 21, 5, 2, 3, 1,
  8, 22, 2, 2, 1, 1,
  8, 23, 2, 2, 2, 1,
  9, 24, 3, 1, 1, 2,
  9, 25, 3, 1, 2, 2,
  9, 26, 3, 1, 3, 2,
  10, 27, 2, 1, 1, 2,
  10, 28, 2, 1, 3, 2,
  11, 29, 3, 1, 1, 2,
  11, 30, 3, 1, 1, 2,
  11, 31, 3, 1, 2, 2,
  12, 32, 3, 2, 1, 2,
  12, 33, 3, 2, 3, 2,
  12, 34, 3, 2, 3, 2,
  13, 35, 3, 2, 2, 2,
  13, 36, 3, 2, 2, 2,
  13, 37, 3, 2, 3, 2,
  14, 38, 2, 2, 1, 2,
  14, 39, 2, 2, 2, 2,
  15, 40, 5, 2, 1, 2,
  15, 41, 5, 2, 1, 2,
  15, 42, 5, 2, 2, 2,
  15, 43, 5, 2, 3, 2,
  15, 44, 5, 2, 3, 2,
  16, 45, 2, 2, 1, 2,
  16, 46, 2, 2, 2, 2
)


hh_ctrl <- tibble::tribble(
  ~ZONE, ~HH_VAR, ~N,
  1, 1, 35,
  1, 2, 65,
  2, 1, 35,
  2, 2, 65,
  3, 1, 35,
  3, 2, 65,
  4, 1, 35,
  4, 2, 65
)

ind_ctrl <- tibble::tribble(
  ~ZONE, ~P_VAR, ~N,
  1, 1, 91,
  1, 2, 65,
  1, 3, 104,
  2, 1, 91,
  2, 2, 65,
  2, 3, 104,
  3, 1, 91,
  3, 2, 65,
  3, 3, 104,
  4, 1, 91,
  4, 2, 65,
  4, 3, 104
)

geo_hierarchy <- tibble::tribble(
  ~REGION, ~ZONE,
  1, 1,
  1, 2,
  2, 3,
  2, 4
)

test_that("Create ml_problem by zones", {
  problems <- ml_problem(
    ref_sample = ref_sample,
    field_names = special_field_names(
      groupId = "HHNR", individualId = "PNR", count = "N",
      zone = "ZONE", region = "REGION"
    ),
    group_controls = list(hh_ctrl),
    individual_controls = list(ind_ctrl),
    geo_hierarchy = geo_hierarchy
  )

  expect_true(all(sapply(problems, is_ml_problem)))
})

test_that("bad zone-by-zone arguments", {
  expect_error(
    ml_problem(
      ref_sample = ref_sample,
      field_names = special_field_names(
        groupId = "HHNR", individualId = "PNR", count = "N",
        zone = "NON-EXISTED-ZONE", region = "REGION"
      ),
      group_controls = list(hh_ctrl, hh_ctrl),
      individual_controls = list(ind_ctrl, ind_ctrl),
      geo_hierarchy = geo_hierarchy
    ),
    regex = "\\{NON-EXISTED-ZONE\\} is not in `geo_hierarchy`"
  )

  expect_error(
    ml_problem(
      ref_sample = ref_sample,
      field_names = special_field_names(
        groupId = "HHNR", individualId = "PNR", count = "N",
        zone = "ZONE", region = "NON-EXISTED-REGION"
      ),
      group_controls = list(hh_ctrl, hh_ctrl),
      individual_controls = list(ind_ctrl, ind_ctrl),
      geo_hierarchy = geo_hierarchy
    ),
    regex = "\\{NON-EXISTED-REGION\\} is not in `geo_hierarchy`"
  )

  expect_error(
    ml_problem(
      ref_sample = ref_sample[, !names(ref_sample) %in% "REGION"],
      field_names = special_field_names(
        groupId = "HHNR", individualId = "PNR", count = "N",
        zone = "ZONE", region = "REGION"
      ),
      group_controls = list(hh_ctrl, hh_ctrl),
      individual_controls = list(ind_ctrl, ind_ctrl),
      geo_hierarchy = geo_hierarchy
    ),
    regex = "\\{REGION\\} is not in `ref_sample`"
  )

  bad_hh_ctrl <- hh_ctrl[hh_ctrl$ZONE != 4, ]
  expect_error(
    ml_problem(
      ref_sample = ref_sample,
      field_names = special_field_names(
        groupId = "HHNR", individualId = "PNR", count = "N",
        zone = "ZONE", region = "REGION"
      ),
      group_controls = list(bad_hh_ctrl),
      individual_controls = list(ind_ctrl, ind_ctrl),
      geo_hierarchy = geo_hierarchy
    ),
    regex = "Zone mismatch between individual and group controls:"
  )
})
