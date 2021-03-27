library(dplyr)
library(tidyr)
xt <- read.table(text = "	0--14	15--34	35--64	65+
nonworker	73	23	35	74
parttime	0	42	17	15
fulltime	0	60	65	2
", check.names = FALSE)

ct1 <- read.table(text = "nonworker	parttime	fulltime
124	83	227
", check.names = FALSE, header = TRUE)

ct2 <- read.table(text = "0--14	15--34	35--64	65+
88	132	115	99
", check.names = FALSE, header = TRUE)

rs <- xt %>%
  as.matrix() %>%
  as.table() %>%
  as.data.frame() %>%
  as_data_frame() %>%
  mutate_each(funs(ofactor = forcats::fct_inorder(.)), Var1, Var2) %>%
  select(-Var1, -Var2) %>%
  rename(WKSTAT = Var1_ofactor, AGE = Var2_ofactor) %>%
  {
    .[rep(seq_along(.$Freq), .$Freq), ]
  } %>%
  select(-Freq) %>%
  `rownames<-`(NULL) %>%
  mutate(PNR = seq_along(WKSTAT) - 1, HHNR = seq_along(WKSTAT) - 1, APER = 1L) %>%
  select(PNR, HHNR, APER, everything())

ct1 <-
  ct1 %>%
  gather(WKSTAT, N) %>%
  mutate(WKSTAT = forcats::fct_inorder(WKSTAT))

ct2 <-
  ct2 %>%
  gather(AGE, N)

fp <- fitting_problem(
  rs, field_names = special_field_names("HHNR", "PNR", count = "N"),
  individual_controls = NULL,
  group_controls = list(ct1, ct2)
)

rextdata::use_extdata(flat = fp)
