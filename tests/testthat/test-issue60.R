test_that("Fixes issue#60 - minimal example", {
    reference_sample <- tibble::tribble(
        ~HHNR, ~PNR, ~CAR, ~WKSTAT,
        1L,   1L,  "0",     "1",
        1L,   2L,  "0",     "2",
        1L,   3L,  "0",     "3",
        2L,   4L,  "0",     "1",
        2L,   5L,  "0",     "3",
        6L,  15L,  "1",     "1",
        6L,  16L,  "1",     "2",
        8L,  22L,  "2",     "1",
        8L,  23L,  "2",     "2"
    )

    individual_control <- tibble::tribble(
        ~WKSTAT, ~N,
        "1", 91L,
        "2", 65L,
        "3", 104L
    )

    group_control <- tibble::tribble(
        ~CAR, ~N,
        "0", 35L,
        "1", 65L,
        "2", 0L
    )

    problem <- ml_problem(
        ref_sample = reference_sample,
        controls = list(
            individual = list(individual_control),
            group = list(group_control)
        ),
        field_names = special_field_names(
            groupId = "HHNR",
            individualId = "PNR",
            count = "N"
        )
    )

    algos <- eval(formals(ml_fit)$algorithm)
    fits <- lapply(algos, function(x) {ml_fit(problem, algorithm = x)})
    for (fit in fits) {
        expect_equal(
            length(fit$flat_weights), 
            length(unique(fit$flat$ml_problem$refSample))
        )
    }
})
