#' @title Combined p-value from Fisher's method
#'
#' @description This function computes the combined p-value based on two
#'     parameter estimates using the Fisher's method (also known as the product
#'     method)
#'
#' @inheritParams p2TR
#'
#' @return The combined p-value based on Fisher's method
#'
#' @author Samuel Pawel
#'
#' @examples
#' ## p-value for H0: logRR = 0 in RESPIRE trials
#' pFisher(mu = 0, t1 = -0.4942, t2 = -0.1847, se1 = 0.1833, se2 = 0.1738,
#'         alternative = "less")
#'
#' @export

pFisher <- function(mu = 0, t1, t2, se1, se2, alternative = "greater") {
    ## check inputs
    checkpfinputs(mu = mu, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                  alternative = alternative)

    ## compute individual p-values
    p1 <- pNormal(mu = mu, t = t1, se = se1, alternative = alternative)
    p2 <- pNormal(mu = mu, t = t2, se = se2, alternative = alternative)

    ## compute combined p-value
    p <- 1 - stats::pchisq(q = -2*(log(p1) + log(p2)), df = 4)
    return(p)
}

#' @title Combined estimation function from Fisher's method
#'
#' @description Computes parameter estimate from estimation function based on
#'     Fisher's method
#'
#' @inheritParams muEdgington
#'
#' @return The parameter estimate based on Fisher's method
#'
#' @author Samuel Pawel
#'
#' @examples
#' ## 95% CI and median estimate for logRR in RESPIRE trials
#' muFisher(a = c(0.975, 0.5, 0.025), t1 = -0.4942, t2 = -0.1847, se1 = 0.1833,
#'          se2 = 0.1738, alternative = "less")
#'
#' @export

muFisher <- function(a = 0.5, t1, t2, se1, se2, alternative = "greater", ...) {
    ## check inputs
    checkefinputs(a = a, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                  alternative = alternative)

    ## compute combined parameter estimate
    est <- sapply(X = a, FUN = function(ai) {
        ## find numerical solution for p(mu) = ai
        res <- muNumerical(p = pFisher, a = ai, t1 = t1, t2 = t2, se1 = se1,
                           se2 = se2, alternative = alternative,
                           searchInt = "adaptive", ... = ...)
        return(res)
    })
    return(est)
}

## ## check algorithm
## t1 <- 2
## t2 <- 3.5
## se1 <- sqrt(2/5)
## se2 <- sqrt(2/10)
## alternative <- "less"
## museq <- seq(t1 - 3*se1, t2 + 3*se2, 0.01)
## plot(x = museq, y = pFisher(museq, t1, t2, se1, se2, alternative), type = "l")
## matlines(x = museq, y = cbind(pNormal(museq, t1, se1, alternative),
##                               pNormal(museq, t2, se2, alternative)),
##          lty = 2, col = 4)
## abline(h = c(0.0025, 0.5, 0.9975), lty = 3)
## abline(v = muFisher(a = c(0.0025, 0.5, 0.9975), t1, t2, se1, se2, alternative), lty = 3)
