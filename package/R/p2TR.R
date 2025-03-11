#' @title Combined p-value from the two-trials rule
#'
#' @description This function computes the combined p-value based on two
#'     parameter estimates using the two-trials rule (also known as the maximum
#'     method)
#'
#' @param mu Null value. Defaults to \code{0}
#' @param t1 Parameter estimate from trial 1
#' @param t2 Parameter estimate from trial 2
#' @param se1 Standard error of the parameter estimate from trial 1
#' @param se2 Standard error of the parameter estimate from trial 2
#' @param alternative One-sided alternative hypothesis. Can be either
#'     \code{"greater"} or \code{"less"}. Defaults to \code{"greater"}
#'
#' @return The combined p-value based on the two-trials rule
#'
#' @author Samuel Pawel
#'
#' @examples
#' ## p-value for H0: logRR = 0 in RESPIRE trials
#' p2TR(mu = 0, t1 = -0.4942, t2 = -0.1847, se1 = 0.1833, se2 = 0.1738,
#'      alternative = "less")
#'
#' @export

p2TR <- function(mu = 0, t1, t2, se1, se2, alternative = "greater") {
    ## check inputs
    checkpfinputs(mu = mu, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                  alternative = alternative)

    ## compute individual p-values
    p1 <- pNormal(mu = mu, t = t1, se = se1, alternative = alternative)
    p2 <- pNormal(mu = mu, t = t2, se = se2, alternative = alternative)

    ## compute combined p-value
    p <- pmax(p1, p2)^2
    return(p)
}

#' @title Combined estimation function from the two-trials rule
#'
#' @description Computes parameter estimate from estimation function based on
#'     the two-trials rule
#'
#' @inheritParams p2TR
#' @param a P-value function quantile corresponding to the parameter estimate.
#'     Defaults to \code{0.5}, which corresponds to the median estimate. Set
#'     to \code{a = c(0.025, 0.975)} to obtain limits of a 95% confidence
#'     interval
#' @param ... Additional arguments (for consistency with other estimation
#'     functions)
#'
#' @return The parameter estimate based on the two-trials rule
#'
#' @author Samuel Pawel
#'
#' @examples
#' ## 95% CI and median estimate for logRR in RESPIRE trials
#' mu2TR(a = c(0.975, 0.5, 0.025), t1 = -0.4942, t2 = -0.1847, se1 = 0.1833,
#'       se2 = 0.1738, alternative = "less")
#'
#' @export

mu2TR <- function(a = 0.5, t1, t2, se1, se2, alternative = "greater", ...) {
    ## check inputs
    checkefinputs(a = a, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                  alternative = alternative)

    ## compute combined parameter estimate
    za <- stats::qnorm(p = sqrt(a))
    if (alternative == "greater") {
        est <- pmin(t1 + se1*za, t2 + se2*za)
    } else {
        est <- pmax(t1 - se1*za, t2 - se2*za)
    }
    return(est)
}
