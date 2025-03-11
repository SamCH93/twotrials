#' @title Check inputs of combined p-value function
#'
#' @description Checks validity of inputs supplied to a combined p-value
#'     function and estimation functions
#'
#' @param mu Vector of null values
#' @param a Vector of p-value function quantiles
#' @param t1 Parameter estimate from trial 1
#' @param t2 Parameter estimate from trial 2
#' @param se1 Standard error of the parameter estimate from trial 1
#' @param se2 Standard error of the parameter estimate from trial 2
#' @param alternative One-sided alternative of the p-value function. Can be
#'     either \code{"greater"} or \code{"less"}
#'
#' @return Returns an error in case the inputs are not valid
#'
#' @author Samuel Pawel
#'
#' @noRd
#'
#' @keywords internal

checkinputs <- function(t1, t2, se1, se2, alternative) {
    stopifnot(length(t1) == 1,
              is.numeric(t1),
              is.finite(t1))

    stopifnot(length(t2) == 1,
              is.numeric(t2),
              is.finite(t2))

    stopifnot(length(se1) == 1,
              is.numeric(se1),
              is.finite(se1),
              se1 > 0)

    stopifnot(length(se2) == 1,
              is.numeric(se2),
              is.finite(se2),
              se2 > 0)

    stopifnot(length(alternative) == 1,
              is.character(alternative),
              !is.na(alternative),
              alternative %in% c("greater", "less"))
}

checkpfinputs <- function(mu, t1, t2, se1, se2, alternative) {
    stopifnot(length(mu) >= 1,
              all(is.numeric(mu)),
              all(is.finite(mu)))

    checkinputs(t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                alternative = alternative)
}

checkefinputs <- function(a, t1, t2, se1, se2, alternative) {
    stopifnot(length(a) >= 1,
              all(is.numeric(a)),
              all(is.finite(a)),
              all(a > 0),
              all(a < 1))

    checkinputs(t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                alternative = alternative)
}

#' @title Normal p-value
#'
#' @description Computes p-value based on normally distributed parameter
#'     estimate and its standard error
#'
#' @param mu Null value
#' @param t Parameter estimate
#' @param se Standard error of the parameter estimate
#' @param alternative Alternative hypothesis. Can be either \code{"greater"},
#'     \code{"less"}, or \code{"two.sided"}. Defaults to \code{"greater"}
#'
#' @return The p-value
#'
#' @author Samuel Pawel
#'
#' @noRd
#'
#' @keywords internal

pNormal <- function(mu, t, se, alternative = "greater") {
    ## compute z-value
    z <- (t - mu)/se

    ## compute p-value
    if (alternative == "greater") {
        p <- 1 - stats::pnorm(q = z)
    } else if (alternative == "less") {
        p <- stats::pnorm(q = z)
    } else {
        p <- 2*(1 - stats::pnorm(q = abs(z)))
    }

    return(p)
}

#' @title Normal parameter estimate
#'
#' @description Computes parameter estimate from normal p-value function
#'
#' @inheritParams pNormal
#' @param alternative Alternative hypothesis. Can be either \code{"greater"}, or
#'     \code{"less"}. Defaults to \code{"greater"}
#' @param a P-value function quantile corresponding to the parameter estimate.
#'     Defaults to \code{a = 0.5}, which corresponds to the median estimate
#'
#' @return The parameter estimate
#'
#' @author Samuel Pawel
#'
#' @noRd
#'
#' @keywords internal

muNormal <- function(a = 0.5, t, se, alternative = "greater") {
    ## compute combined parameter estimate
   za <- stats::qnorm(p = a)
    if (alternative == "greater") {
        est <- t + za*se
    } else if (alternative == "less") {
        est <- t - za*se
    } else {
        stop('alternative needs to be "greater" or "less"')
    }

    return(est)
}

#' @title Numerical quantile search for combined p-value functions
#'
#' @description This function numerically computes a combined p-value function
#'     quantile
#'
#' @inheritParams mu2TR
#' @param p A combined p-value function. Can be either \code{pEdgington},
#'     \code{pFisher}, \code{pPearson}, \code{p2TR}, or \code{pMA}
#' @param searchInt An interval where the numerical search should be performed
#'     or \code{"adaptive"} to choose the interval adaptively. Defaults to
#'     \code{"adaptive"}
#' @param ... Additional arguments for \code{\link{stats::uniroot}}
#'
#' @return The parameter estimate
#'
#' @author Samuel Pawel
#'
#' @noRd
#'
#' @keywords internal

muNumerical <- function(p, a, t1, t2, se1, se2, alternative,
                        searchInt = "adaptive", ...) {
    ## find numerical solution for p(mu) = a
    rootFun <- function(mu) {
        p(mu = mu, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
          alternative = alternative) - a
    }
    if (length(searchInt) != 2) { # adaptive strategy
        mu1 <- muNormal(a = a, t = t1, se = se1, alternative = alternative)
        mu2 <- muNormal(a = a, t = t2, se = se2, alternative = alternative)
        searchInt <- c(pmin(mu1, mu2) - 3*pmax(se1, se2),
                       pmax(mu1, mu2) + 3*pmax(se1, se2))
    }
    res <- try(stats::uniroot(f = rootFun, interval = searchInt,
                              extendInt = "yes", ... = ...)$root)
    if (inherits(res, "try-error")) {
        warnstr <- paste0("Problems in finding root of p(mu) = ", a)
        warning(warnstr)
        return(NaN)
    } else {
        return(res)
    }
}
