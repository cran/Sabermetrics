\name{wrc}
\alias{wrc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted Runs Created}
\description{
Weighted Runs Created is a statistic which attempts to credit a hitter for the value of each outcome.}
\usage{
wrc(wOBA, PA, year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wOBA}{
Weighted On Base Average}
  \item{PA}{
Plate Appearances}
  \item{year}{
Season}
}
\details{
wRC is reported with runs as the units}
\value{
Returns a value equal to (((wOBA-linearWeights$wOBA[which(linearWeights$Season == year)])/linearWeights$wOBAScale[which(linearWeights$Season == year)])+weights$RPerPA[which(linearWeights$Season == year)])*PA
}
\references{
http://www.fangraphs.com/library/offense/wrc/}
\author{
Peter Xenopoulos}
\note{
wRC is considered a better representation of offensive value than something like \link{ops}}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{woba},\link{wraa},\link{wrcplus}}
\examples{
## Let's calculate Mike Trout's 2014 wRC
## He had a wOBA of .402 and 705 PA's
## We should return 125.879
wrc(.402,705,2014)

## The function is currently defined as
function (wOBA, PA, year) 
{
    wrc <- (((wOBA - linearWeights$wOBA[which(linearWeights$Season == 
        year)])/linearWeights$wOBAScale[which(linearWeights$Season == 
        year)]) + weights$RPerPA[which(linearWeights$Season == 
        year)]) * PA
    return(wrc)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ wrc }
\keyword{ weighted runs created }% __ONLY ONE__ keyword per line
