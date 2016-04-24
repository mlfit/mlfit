ref_sample <- tibble::tibble(
  ~HHNR, ~PNR, ~H, ~P,
  1,     1,    1,  1)

group <- tibble::tibble(
  ~H, ~N,
  1,  1)

ind <- tibble::tibble(
  ~P, ~N,
  1,  2)

fp <- fitting_problem(ref_sample = ref_sample,
                      individual_controls = list(ind),
                      group_controls = list(group),
                      field_names = special_field_names("HHNR", "PNR", count = "N"))

saveRDS(fp, "inst/extdata/Conflict.rds")
