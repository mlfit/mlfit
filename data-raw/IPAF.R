ex <- toy_example()

exd <- lapply(ex, import_IPAF_results, all_weights = TRUE)

names(exd) <- file.path("inst/extdata", paste0(names(exd), ".rds"))

mapply(saveRDS, exd, names(exd))
