\name{linearWeights}
\alias{linearWeights}
\docType{data}
\title{
Linear Weights Dataset
}
\description{
This is a dataset containing useful yearly metrics from FanGraphs.
}
\usage{linearWeights}
\format{
  A data frame with seasonal data going back to 1871. Contains 14 variables.
  \describe{
    \item{\code{Season}}{1871-Present}
    \item{\code{wOBA}}{Season Average wOBA}
    \item{\code{wOBAScale}}{Season wOBA Scaling Factor}
    \item{\code{wBB}}{Weight of an walk}
    \item{\code{wHBP}}{Weight of a hit by pitch}
    \item{\code{w1B}}{Weight of a single}
    \item{\code{w2B}}{Weight of a double}
    \item{\code{w3B}}{Weight of a triple}
    \item{\code{wHR}}{Weight of a home run}
    \item{\code{runSB}}{Weight of a stolen base}
    \item{\code{runCS}}{Weight of being caught stealing}
    \item{\code{RPerPA}}{Runs Per Plate Appearance}
    \item{\code{RPerW}}{Runs Per Win}
    \item{\code{cFIP}}{Season FIP constant}
  }
}
\details{
This dataset is useful for calculating season-dependent metrics such as wOBA and FIP.
}
\source{
http://www.fangraphs.com/guts.aspx?type=cn
}
\references{
http://www.fangraphs.com/
}
\examples{
## Give the last 5 years
head(linearWeights, 5)
}
\keyword{datasets}
