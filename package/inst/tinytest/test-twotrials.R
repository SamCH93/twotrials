library(twotrials)
library(tinytest)

## logRR estimates from RESPIRE trials
res <- twotrials(null = 0,  t1 = -0.4942, t2 = -0.1847, se1 = 0.1833,
                 se2 = 0.1738, alternative = "less", level = 0.95)

## check that CI and median estimate correctly determined
for (i in seq_along(res$pfuns)) {
    f <- res$pfuns[[i]]
    mtd <- names(res$pfuns)[i]

    print(expect_equal(f(res$summaries$lower[i]), 0.975,
                       info = paste0(mtd, ": p(lower limit) should be 0.975"),
                       tol = 0.001))

    print(expect_equal(f(res$summaries$est[i]), 0.5,
                       info = paste0(mtd, ": p(median estimate) should be 0.5"),
                       tol = 0.001))

    print(expect_equal(f(res$summaries$upper[i]), 0.025,
                       info = paste0(mtd, ": p(upper CI limit) should be 0.025"),
                       tol = 0.001))
}
