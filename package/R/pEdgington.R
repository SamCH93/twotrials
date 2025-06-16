#' @title Combined p-value from Edgington's method
#'
#' @description This function computes the combined p-value based on two
#'     parameter estimates using Edgington's method (also known as the sum
#'     method)
#'
#' @inheritParams p2TR
#'
#' @return The combined p-value based on Edgington's method
#'
#' @author Samuel Pawel
#'
#' @seealso \code{\link{muEdgington}}
#'
#' @examples
#' ## p-value for H0: logRR = 0 in RESPIRE trials
#' pEdgington(mu = 0, t1 = -0.4942, t2 = -0.1847, se1 = 0.1833, se2 = 0.1738,
#'            alternative = "less")
#'
#' @export

pEdgington <- function(mu = 0, t1, t2, se1, se2, alternative = "greater") {
    ## check inputs
    checkpfinputs(mu = mu, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                  alternative = alternative)

    ## compute individual p-values
    p1 <- pNormal(mu = mu, t = t1, se = se1, alternative = alternative)
    p2 <- pNormal(mu = mu, t = t2, se = se2, alternative = alternative)

    ## compute combined p-value
    e <- p1 + p2
    p <- ifelse(e <= 1, 0.5*e^2, 1 - 0.5*(2 - e)^2)
    return(p)
}

#' @title Combined estimation function from Edgington's method
#'
#' @description This function computes parameter estimates from the combined
#'     estimation function based on Edgington's method
#'
#' @inheritParams mu2TR
#' @param ... Additional arguments for \code{stats::uniroot}
#'
#' @return The parameter estimate based on Edgington's method
#'
#' @author Samuel Pawel
#'
#' @seealso \code{\link{pEdgington}}
#'
#' @examples
#' ## 95% CI and median estimate for logRR in RESPIRE trials
#' muEdgington(a = c(0.975, 0.5, 0.025), t1 = -0.4942, t2 = -0.1847, se1 = 0.1833,
#'             se2 = 0.1738, alternative = "less")
#'
#' @export

muEdgington <- function(a = 0.5, t1, t2, se1, se2, alternative = "greater", ...) {
    ## check inputs
    checkefinputs(a = a, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                  alternative = alternative)

    est <- sapply(X = a, FUN = function(ai) {
        if (ai == 0.5) {
            ## analytical solution for median estimate
            medest <- (t1/se1 + t2/se2)/(1/se1 + 1/se2)
            return(medest)
        } else {
            ## numerical solution for other quantiles
            res <- muNumerical(p = pEdgington, a = ai, t1 = t1, t2 = t2,
                               se1 = se1, se2 = se2, alternative = alternative,
                               searchInt = "adaptive", ... = ...)
            return(res)
        }
    })
    return(est)
}

## ## check algorithm
## t1 <- 1
## t2 <- 3.5
## se1 <- sqrt(2/50)
## se2 <- sqrt(2/100)
## alternative <- "greater"
## museq <- seq(t1 - 3*se1, t2 + 3*se2, 0.01)
## plot(x = museq, y = pEdgington(museq, t1, t2, se1, se2, alternative), type = "l")
## matlines(x = museq, y = cbind(pNormal(museq, t1, se1, alternative),
##                               pNormal(museq, t2, se2, alternative)),
##          lty = 2, col = 4)
## abline(h = c(0.0025, 0.5, 0.9975), lty = 3)
## abline(v = muEdgington(a = c(0.0025, 0.5, 0.9975), t1, t2, se1, se2, alternative),
##        lty = 3)
