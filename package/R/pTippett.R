#' @title Combined p-value from Tippett's method
#'
#' @description This function computes the combined p-value based on two
#'     parameter estimates using Tippett's method (also known as the minimum
#'     method)
#'
#' @inheritParams p2TR
#'
#' @return The combined p-value based on Tippett's method
#'
#' @author Samuel Pawel
#'
#' @examples
#' ## p-value for H0: logRR = 0 in RESPIRE trials
#' pTippett(mu = 0, t1 = -0.491, t2 = -0.185, se1 = 0.179, se2 = 0.174,
#'          alternative = "less")
#'
#' @export

pTippett <- function(mu = 0, t1, t2, se1, se2, alternative = "greater") {
    ## check inputs
    checkpfinputs(mu = mu, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                  alternative = alternative)

    ## compute individual p-values
    p1 <- pNormal(mu = mu, t = t1, se = se1, alternative = alternative)
    p2 <- pNormal(mu = mu, t = t2, se = se2, alternative = alternative)

    ## compute combined p-value
    p <- 1 - (1 - pmin(p1, p2))^2
    return(p)
}

#' @title Combined estimation function from Tippett's method
#'
#' @description Computes parameter estimate from estimation function based on
#'     Tippett's method
#'
#' @inheritParams mu2TR
#'
#' @return The parameter estimate based on Tippett's method
#'
#' @author Samuel Pawel
#'
#' @examples
#' ## 95% CI and median estimate for logRR in RESPIRE trials
#' muTippett(a = c(0.975, 0.5, 0.025), t1 = -0.491, t2 = -0.185, se1 = 0.179,
#'           se2 = 0.174, alternative = "less")
#'
#' @export

muTippett <- function(a = 0.5, t1, t2, se1, se2, alternative = "greater", ...) {
    ## check inputs
    checkefinputs(a = a, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                  alternative = alternative)

    ## compute combined parameter estimate
    za <- stats::qnorm(p = sqrt(1 - a))
    if (alternative == "greater") {
        est <- pmax(t1 - se1*za, t2 - se2*za)
    } else {
        est <- pmin(t1 + se1*za, t2 + se2*za)
    }
    return(est)
}
