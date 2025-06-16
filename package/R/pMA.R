#' @title Combined p-value from fixed-effect meta-analysis
#'
#' @description This function computes the combined p-value based on two
#'     parameter estimates using fixed-effect meta-analysis (equivalent to
#'     Stouffer's p-value combination method with suitable weights)
#'
#' @inheritParams p2TR
#'
#' @return The combined p-value based on fixed-effect meta-analysis
#'
#' @author Samuel Pawel
#'
#' @seealso \code{\link{pMA}}
#'
#' @examples
#' ## p-value for H0: logRR = 0 in RESPIRE trials
#' pMA(mu = 0, t1 = -0.4942, t2 = -0.1847, se1 = 0.1833, se2 = 0.1738,
#'     alternative = "less")
#'
#' @export

pMA <- function(mu = 0, t1, t2, se1, se2, alternative = "greater") {
    ## check inputs
    checkpfinputs(mu = mu, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                  alternative = alternative)

    ## compute pooled standard error and estimate
    sepool <- 1/sqrt(1/se1^2 + 1/se2^2)
    tpool <- (t1/se1^2 + t2/se2^2)*sepool^2

    ## compute combined p-value
    p <- pNormal(mu = mu, t = tpool, se = sepool, alternative = alternative)
    return(p)
}


#' @title Combined estimation function from fixed-effect meta-analysis
#'
#' @description This function computes parameter estimates from the combined
#'     estimation function based on fixed-effect meta-analysis
#'
#' @inheritParams mu2TR
#'
#' @return The parameter estimate based on fixed-effect meta-analysis
#'
#' @author Samuel Pawel
#'
#' @seealso \code{\link{muMA}}
#'
#' @examples
#' ## 95% CI and median estimate for logRR in RESPIRE trials
#' muMA(a = c(0.975, 0.5, 0.025), t1 = -0.4942, t2 = -0.1847, se1 = 0.1833,
#'      se2 = 0.1738, alternative = "less")
#'
#' @export

muMA <- function(a = 0.5, t1, t2, se1, se2, alternative = "greater", ...) {
    ## check inputs
    checkefinputs(a = a, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                  alternative = alternative)

    ## compute pooled standard error and estimate
    sepool <- 1/sqrt(1/se1^2 + 1/se2^2)
    tpool <- (t1/se1^2 + t2/se2^2)*sepool^2

    ## compute combined parameter estimate
    est <- muNormal(a = a, t = tpool, se = sepool, alternative = alternative)
    return(est)
}
