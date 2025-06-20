% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twotrials.R
\name{print.twotrials}
\alias{print.twotrials}
\title{Print method for class \code{"twotrials"}}
\usage{
\method{print}{twotrials}(x, digits = 3, ...)
}
\arguments{
\item{x}{Object of class \code{"twotrials"}}

\item{digits}{Number of digits for formatting of numbers}

\item{...}{Other arguments (for consistency with the generic)}
}
\value{
Prints text summary in the console and invisibly returns the
    \code{"twotrials"} object
}
\description{
Print method for class \code{"twotrials"}
}
\examples{
## logRR estimates from RESPIRE trials
twotrials(null = 0, t1 = -0.4942, t2 = -0.1847, se1 = 0.1833, se2 = 0.1738,
          alternative = "less", level = 0.95)

}
\seealso{
\code{\link{twotrials}}
}
\author{
Samuel Pawel
}
