ye_flat <- read.table(text = "1	0	1	1	1
1	0	1	0	1
1	0	2	1	0
0	1	1	0	2
0	1	0	2	1
0	1	1	1	0
0	1	2	1	2
0	1	1	1	0
")

ye <- tibble::tribble(
  ~HHNR, ~APER, ~HH_VAR, ~P_VAR,
  1, 3, 1, 1,
  1, 3, 1, 2,
  1, 3, 1, 3,
  2, 2, 1, 1,
  2, 2, 1, 3,
  3, 3, 1, 1,
  3, 3, 1, 1,
  3, 3, 1, 2,
  4, 3, 2, 1,
  4, 3, 2, 3,
  4, 3, 2, 3,
  5, 3, 2, 2,
  5, 3, 2, 2,
  5, 3, 2, 3,
  6, 2, 2, 1,
  6, 2, 2, 2,
  7, 5, 2, 1,
  7, 5, 2, 1,
  7, 5, 2, 2,
  7, 5, 2, 3,
  7, 5, 2, 3,
  8, 2, 2, 1,
  8, 2, 2, 2
)

ye_hh <- tibble::tribble(
  ~HH_VAR, ~N,
  1,       35,
  2,       65
)

ye_ind <- tibble::tribble(
  ~P_VAR, ~N,
  1, 91,
  2, 65,
  3, 104
)

ye_problem <- fitting_problem(
  ref_sample = ye,
  field_names = special_field_names(
    groupId = "HHNR", individualId = "PNR",
    count = "N"
  ),
  group_controls = list(ye_hh),
  individual_controls = list(ye_ind)
)

rextdata::use_extdata(ye = ye_problem, overwrite = TRUE)
