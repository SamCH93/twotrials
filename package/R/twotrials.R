#' @title Combined p-value function inference for two trials
#'
#' @description This function computes combined p-values, point estimates, and
#'     confidence intervals based on two parameter estimates and using
#'     fixed-effect meta-analysis, the two-trials rule, Edgington's, Fisher's,
#'     Pearson's, and Tippett's p-value functions.
#'
#' @inheritParams p2TR
#' @param null Null value for which p-values should be computed. Defaults to
#'     \code{0}
#' @param level Confidence interval level. Defaults to \code{0.95}
#'
#' @return Object of class \code{"twotrials"}, which is a list of the supplied
#'     arguments augmented with \code{pfuns} and \code{ipfuns} (combined and
#'     individual p-value functions), \code{mufuns} and \code{imufuns} (combined
#'     and individual estimation functions), and \code{summaries} and
#'     \code{isummaries} (combined and individual confidence intervals, point
#'     estimates, p-values) elements
#'
#' @author Samuel Pawel
#'
#' @examples
#' ## logRR estimates from RESPIRE trials
#' twotrials(null = 0, t1 = -0.4942, t2 = -0.1847, se1 = 0.1833, se2 = 0.1738,
#'           alternative = "less", level = 0.95)
#'
#' ## compute 99.875% CIs instead
#' twotrials(null = 0, t1 = -0.4942, t2 = -0.1847, se1 = 0.1833, se2 = 0.1738,
#'           alternative = "less", level = 0.99875)
#'
#' @export

twotrials <- function(null = 0, t1, t2, se1, se2, alternative = "greater",
                      level = 0.95) {
    ## check inputs
    checkpfinputs(mu = null, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                  alternative = alternative)
    stopifnot(length(level) == 1,
              is.numeric(level),
              is.finite(level),
              level < 1,
              level > 0.5)

    ## compute summaries and p-value functions for individual trials
    t <- c(t1, t2)
    se <- c(se1, se2)
    trialnames <- c("Trial 1", "Trial 2")
    trialnamesshort <- c("T1", "T2")
    iresList <- lapply(X = seq_along(t), FUN = function(i) {
        pf <- function(mu) {
            pNormal(mu = mu, t = t[i], se = se[i], alternative = alternative)
        }
        mf <- function(a) {
            muNormal(a = a, t = t[i], se = se[i], alternative = alternative)
        }
        p0 <- pf(mu = null) # combined p-value
        medest <- mf(a = 0.5) # median estimate
        ci <- mf(a = (1 + c(-1, 1)*level)*0.5) # CI
        sumDF <- data.frame(trial = trialnames[i], lower = min(ci),
                            est = medest, upper = max(ci), p0 = p0)
        out <- list("pf" = pf, "mf" = mf, "summary" = sumDF)
    })
    ipfuns <- lapply(X = iresList, FUN = function(x) x$pf)
    names(ipfuns) <- trialnamesshort
    imufuns <- lapply(X = iresList, FUN = function(x) x$mf)
    names(imufuns) <- trialnamesshort
    isummaries <- do.call("rbind",
                         lapply(X = iresList, FUN = function(x) x$summary))

    ## compute summaries and p-value functions for every combination method
    cpfs <- list(p2TR, pMA, pTippett, pFisher, pPearson, pEdgington)
    cmufs <- list(mu2TR, muMA, muTippett, muFisher, muPearson, muEdgington)
    mtdnamesshort <- c("2TR", "MA", "Tippett", "Fisher", "Pearson", "Edgington")
    mtdnames <- c("Two-trials rule", "Meta-analysis", "Tippett",
                  "Fisher", "Pearson", "Edgington")
    cresList <- lapply(X = seq_along(cpfs), FUN = function(i) {
        ## combined p-value function and estimation function
        pf <- function(mu) {
            cpfs[[i]](mu = mu, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                alternative = alternative)
        }
        mf <- function(a, ...) {
            cmufs[[i]](a = a, t1 = t1, t2 = t2, se1 = se1, se2 = se2,
                alternative = alternative, ... = ...)
        }
        p0 <- pf(mu = null) # combined p-value
        medest <- mf(a = 0.5) # median estimate
        ci <- mf(a = (1 + c(-1, 1)*level)*0.5) # CI
        sumDF <- data.frame(method = mtdnames[i], lower = min(ci), est = medest,
                            upper = max(ci), p0 = p0)
        out <- list("pf" = pf, "mf" = mf, "summary" = sumDF)
        return(out)
    })
    pfuns <- lapply(X = cresList, FUN = function(x) x$pf)
    names(pfuns) <- mtdnamesshort
    mufuns <- lapply(X = cresList, FUN = function(x) x$mf)
    names(mufuns) <- mtdnamesshort
    summaries <- do.call("rbind",
                         lapply(X = cresList, FUN = function(x) x$summary))

    ## put everything together
    res <- list("null" = null, "t1" = t1, "t2" = t2, "se1" = se1, "se2" = se2,
                "alternative" = alternative, "level" = level, "pfuns" = pfuns,
                "ipfuns" = ipfuns, "mufuns" = mufuns, "imufuns" = imufuns,
                "summaries" = summaries, "isummaries" = isummaries)
    class(res) <- "twotrials"
    return(res)
}


#' Print method for class \code{"twotrials"}
#' @method print twotrials
#'
#' @param x Object of class \code{"twotrials"}
#' @param digits Number of digits for formatting of numbers
#' @param ... Other arguments (for consistency with the generic)
#'
#' @return Prints text summary in the console and invisibly returns the
#'     \code{"twotrials"} object
#'
#' @author Samuel Pawel
#'
#' @examples
#' ## logRR estimates from RESPIRE trials
#' twotrials(null = 0, t1 = -0.4942, t2 = -0.1847, se1 = 0.1833, se2 = 0.1738,
#'           alternative = "less", level = 0.95)
#'
#' @export
print.twotrials <- function(x, digits = 3, ...) {
    ## results of individual trials
    cat("INDIVIDUAL RESULTS\n")
    isummaries <- x$isummaries
    names(isummaries) <- c("Trial", "Lower CI", "Estimate", "Upper CI",
                          "P-value")
    print(isummaries, digits = digits, row.names = FALSE)
    cat("\n")

    ## results of combined trials
    cat("COMBINED RESULTS\n")
    summaries <- x$summaries
    names(summaries) <- c("Method", "Lower CI", "Estimate", "Upper CI",
                          "P-value")
    print(summaries, digits = digits, row.names = FALSE)
    cat("\n")

    ## additional notes
    note <- paste(paste0("Confidence level: ",
                         signif(100*x$level, digits = 5), "%"),
                  paste0("Null value: ", signif(x$null, digits = digits)),
                  paste0("Alternative: ", x$alternative),
                  sep = "\n")
    cat("NOTES \n", note, "\n", sep = "")

    invisible(x)
}


#' Plot method for class \code{"twotrials"}
#' @method plot twotrials
#'
#' @param x Object of class \code{"twotrials"}
#' @param xlim x-axis limits. Defaults to the confidence interval range of trial
#'     1 and trial 2
#' @param two.sided Logical indicating whether the p-value functions should be
#'     converted to a two-sided p-value function via the centrality function
#'     2min(p, 1 - p). Defaults to \code{FALSE}
#' @param plot Logical indicating whether p-value functions should be plotted.
#'     Defaults to \code{TRUE}
#' @param ... Other arguments (for consistency with the generic)
#'
#' @return Plots p-value functions and invisibly returns a data frames
#'     containing the data underlying the plot
#'
#' @author Samuel Pawel
#'
#' @examples
#' ## logRR estimates from RESPIRE trials
#' res <- twotrials(null = 0, t1 = -0.4942, t2 = -0.1847, se1 = 0.1833, se2 = 0.1738,
#'                  alternative = "less", level = 0.95)
#' plot(res) # one-sided p-value functions
#' plot(res, two.sided = TRUE) # two-sided p-value functions
#'
#' @export
plot.twotrials <- function(x,
                           xlim = c(min(x$isummaries$lower), max(x$isummaries$upper)),
                           two.sided = FALSE, plot = TRUE, ...) {
    ## compute p-values
    museq <- seq(xlim[1], xlim[2], length.out = 1000)
    ip <- sapply(X = x$ipfuns, FUN = function(pf) pf(museq))
    cp <- sapply(X = x$pfuns, FUN = function(pf) pf(museq))
    p <- cbind(ip, cp)

    ## convert to two-sided p-values if specified
    if (two.sided == TRUE) {
        p <- apply(X = p, MARGIN = 2, FUN = function(pi) {
            2*pmin(pi, 1 - pi)
        })
    }

    ## plot p-value functions if specified
    lty <- c(rep(2, 2), rep(1, ncol(cp)))
    cols <- c("#CC79A7F2", "#888888F2", "#000000F2", "#56B4E9F2", "#E69F00F2",
              "#F0E442F2", "#0072B2F2", "#009E73F2")
    labs <- c(x$isummaries$trial, x$summaries$method)
    if (plot == TRUE) {
        graphics::matplot(museq, p, type = "l", col = cols, lty = lty, las = 1,
                          lwd = 1.5, xlab = "Parameter value",
                          ylab = bquote(italic(P) * "-value"), ylim = c(0, 1))
        ## if (two.sided == TRUE) {
        ##     hlines <- 1 - x$level
        ## } else {
        ##     hlines <- (1 + c(-1, 1)*x$level)*0.5
        ## }
        ## abline(h = hlines, lty = 3, col = "#00000080")
        graphics::legend("right", legend = labs, col = cols, lty = lty,
                         lwd = 1.5, cex = 0.75)
    }

    ## convert to long format
    plotDF <- do.call("rbind", lapply(X = seq(1, ncol(p)), FUN = function(i) {
        data.frame(method = labs[i], mu = museq, p = p[,i])
    }))

    invisible(plotDF)
}
