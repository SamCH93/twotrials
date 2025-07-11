% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twotrials.R
\name{twotrials}
\alias{twotrials}
\title{Combined p-value function inference for two trials}
\usage{
twotrials(null = 0, t1, t2, se1, se2, alternative = "greater", level = 0.95)
}
\arguments{
\item{null}{Null value for which p-values should be computed. Defaults to
\code{0}}

\item{t1}{Parameter estimate from trial 1}

\item{t2}{Parameter estimate from trial 2}

\item{se1}{Standard error of the parameter estimate from trial 1}

\item{se2}{Standard error of the parameter estimate from trial 2}

\item{alternative}{One-sided alternative hypothesis. Can be either
\code{"greater"} or \code{"less"}. Defaults to \code{"greater"}}

\item{level}{Confidence interval level. Defaults to \code{0.95}}
}
\value{
Object of class \code{"twotrials"}, which is a list of the supplied
    arguments augmented with \code{pfuns} and \code{ipfuns} (combined and
    individual p-value functions), \code{mufuns} and \code{imufuns} (combined
    and individual estimation functions), and \code{summaries} and
    \code{isummaries} (combined and individual confidence intervals, point
    estimates, p-values, implicit weights) elements
}
\description{
This function computes combined p-values, point estimates, and
    confidence intervals based on two parameter estimates using fixed-effect
    meta-analysis, the two-trials rule, Edgington's, Fisher's, Pearson's, and
    Tippett's combination methods
}
\examples{
## logRR estimates from RESPIRE trials
twotrials(null = 0, t1 = -0.4942, t2 = -0.1847, se1 = 0.1833, se2 = 0.1738,
          alternative = "less", level = 0.95)

## compute 99.875\% CIs instead
twotrials(null = 0, t1 = -0.4942, t2 = -0.1847, se1 = 0.1833, se2 = 0.1738,
          alternative = "less", level = 0.99875)

}
\seealso{
\code{\link{pEdgington}}, \code{\link{muEdgington}},
    \code{\link{pMA}}, \code{\link{muMA}}, \code{\link{pTippett}},
    \code{\link{muTippett}}, \code{\link{p2TR}}, \code{\link{mu2TR}},
    \code{\link{pFisher}}, \code{\link{muFisher}}, \code{\link{pPearson}},
    \code{\link{muPearson}}, \code{\link{plot.twotrials}},
    \code{\link{print.twotrials}}
}
\author{
Samuel Pawel
}
