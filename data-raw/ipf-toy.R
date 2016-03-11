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
  as.matrix %>%
  as.table %>%
  as.data.frame %>%
  as_data_frame %>%
  rename(WKSTAT = Var1, AGE = Var2) %>%
  {.[rep(seq_along(.$Freq), .$Freq), ] } %>%
  select(-Freq) %>%
  `rownames<-`(NULL) %>%
  mutate(PNR = seq_along(WKSTAT) - 1, HHNR = seq_along(WKSTAT) - 1, APER = 1L) %>%
  select(PNR, HHNR, APER, everything())

ct1 <-
  ct1 %>%
  gather(WKSTAT, N)

ct2 <-
  ct2 %>%
  gather(AGE, N)

root <- "inst/extdata/flat"
dir.create(root, showWarnings = FALSE)
write.table(rs, file.path(root, "RS.dat"), sep = "\t", row.names = FALSE, quote = FALSE)
write.table(ct1, file.path(root, "GROUP1.dat"), sep = "\t", row.names = FALSE, quote = FALSE)
write.table(ct2, file.path(root, "GROUP2.dat"), sep = "\t", row.names = FALSE, quote = FALSE)
