library(dplyr)
library(tidyr)

split_control <- function(control) {
  control %>%
    nest(N, DUMMY) %>%
    mutate(N = lapply(data, "[[", "N")) %>%
    select(-data) %>%
    as.data.frame
}

split_grouped <- function(name) {
  x <- readRDS(toy_example(name))

  x$refSample <- x$refSample %>% filter(DUMMY == DUMMY[[1]]) %>% select(-DUMMY) %>% as.data.frame
  x$controls$group <- lapply(x$controls$group, split_control)
  x$controls$individual <- lapply(x$controls$individual, split_control)
  x$algorithms <- NULL
  x$weights <- NULL

  split_name <- gsub("Grouped", "Split", name)
  saveRDS(x, file.path("inst/extdata", paste0(split_name, ".rds")))
}

split_grouped("Joint-Grouped")
