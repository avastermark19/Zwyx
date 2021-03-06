% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ratiosCorrelation.R
\name{countReadsMara3}
\alias{countReadsMara3}
\title{countReadsMara3 Internal function to calculate ratiosCorrelation.}
\usage{
countReadsMara3(scaffolds, G_RESULT2)
}
\arguments{
\item{scaffolds}{Choice of scaffold.}

\item{G_RESULT2}{G_RESULT file.}
}
\value{
Returns ratiosCorrelation data.
}
\description{
This function generates ratiosCorrelation underlying data.
}
\details{
This function returns data from internal function to calculate ratiosCorrelation.
}
\examples{
data(G.RESULT)
countReadsMara3(scaffolds='DPSCF300002', G_RESULT2=G_RESULT)
}
\references{
The "Zwyx" package, 2018 (in press).
}
\author{
AJ Vaestermark, JR Walters.
}
