\name{wraa}
\alias{wraa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted Runs Above Average}
\description{
wRAA measure the number of offensive runs a player contributes compared to an average player.}
\usage{
wraa(woba, year, PA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{woba}{
Weighted On Base Percentage. See \link{woba}}
  \item{year}{
Season}
  \item{PA}{
Plate Appearances}
}
\details{
wRAA is league adjusted, meaning you can use it to compare players from different leagues and years.}
\value{
Returns a value equal to ((wOBA - league wOBA) / wOBA scale) x PA
}
\references{
http://www.fangraphs.com/library/offense/wraa/}
\author{
Peter Xenopoulos}
\note{
Usually, Ten wRAA is equivalent to +1 win.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{woba}}
\examples{
## Let's calculate Mike Trout's 2014 wRAA
## He had a wOBA of .402 and 705 PA's
## We should return 49.74
wraa(.402,2014,705)

## The function is currently defined as
function (woba, year, PA) 
{
    wraa <- ((woba - linearWeights$wOBA[which(linearWeights$Season == 
        year)])/weights$wOBAScale[which(linearWeights$Season == 
        year)]) * PA
    return(wraa)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ wraa }
\keyword{ runs above average }
\keyword{ raa }% __ONLY ONE__ keyword per line
