% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pTippett.R
\name{pTippett}
\alias{pTippett}
\title{Combined p-value from Tippett's method}
\usage{
pTippett(mu = 0, t1, t2, se1, se2, alternative = "greater")
}
\arguments{
\item{mu}{Null value. Defaults to \code{0}}

\item{t1}{Parameter estimate from trial 1}

\item{t2}{Parameter estimate from trial 2}

\item{se1}{Standard error of the parameter estimate from trial 1}

\item{se2}{Standard error of the parameter estimate from trial 2}

\item{alternative}{One-sided alternative hypothesis. Can be either
\code{"greater"} or \code{"less"}. Defaults to \code{"greater"}}
}
\value{
The combined p-value based on Tippett's method
}
\description{
This function computes the combined p-value based on two
    parameter estimates using Tippett's method (also known as the minimum
    method)
}
\examples{
## p-value for H0: logRR = 0 in RESPIRE trials
pTippett(mu = 0, t1 = -0.491, t2 = -0.185, se1 = 0.179, se2 = 0.174,
         alternative = "less")

}
\seealso{
\code{\link{muTippett}}
}
\author{
Samuel Pawel
}
