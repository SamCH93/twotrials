#' @title Combined p-value from Pearson's method
#'
#' @description This function computes the combined p-value based on two
#'     parameter estimates using Pearson's method
#'
#' @inheritParams p2TR
#'
#' @return The combined p-value based on Pearson's method
#'
#' @author Samuel Pawel
#'
#' @examples
#' ## p-value for H0: logRR = 0 in RESPIRE trials
#' pPearson(mu = 0, t1 = -0.491, t2 = -0.185, se1 = 0.179, se2 = 0.174,
#'          alternative = "less")
#'
#' @export

pPearson <- function(mu = 0, t1, t2, se1, se2, alternative = "greater") {
    ## check inputs
    checkpfinputs(mu = mu, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                  alternative = alternative)

    ## compute individual p-values
    p1 <- pNormal(mu = mu, t = t1, se = se1, alternative = alternative)
    p2 <- pNormal(mu = mu, t = t2, se = se2, alternative = alternative)

    ## compute combined p-value
    p <- stats::pchisq(q = -2*(log(1 - p1) + log(1 - p2)), df = 4)
    return(p)
}

#' @title Combined estimation function from Pearson's method
#'
#' @description Computes parameter estimate from estimation function based on
#'     Pearson's method
#'
#' @inheritParams muEdgington
#'
#' @return The parameter estimate based on Pearson's method
#'
#' @author Samuel Pawel
#'
#' @examples
#' ## 95% CI and median estimate for logRR in RESPIRE trials
#' muPearson(a = c(0.975, 0.5, 0.025), t1 = -0.491, t2 = -0.185, se1 = 0.179,
#'           se2 = 0.174, alternative = "less")
#'
#' @export

muPearson <- function(a = 0.5, t1, t2, se1, se2, alternative = "greater", ...) {
    ## check inputs
    checkefinputs(a = a, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                  alternative = alternative)

    ## compute combined parameter estimate
    est <- sapply(X = a, FUN = function(ai) {
        ## find numerical solution for p(mu) = ai
        res <- muNumerical(p = pPearson, a = ai, t1 = t1, t2 = t2, se1 = se1,
                           se2 = se2, alternative = alternative, ... = ...)
        return(res)
    })
    return(est)
}
