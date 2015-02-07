\name{iso}
\alias{iso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Isolated Power}
\description{
Isolated power is a statistic to measure a hitter's raw power}
\usage{
iso(slg, avg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{slg}{
Slugging Percentage. Found from \link{slg}}
  \item{avg}{
Batting Average}
}
\value{
Returns Slugging Percentage - Batting Average
}
\references{
\cite{http://en.wikipedia.org/wiki/Isolated_Power}}
\author{
Peter Xenopoulos }

\examples{
## The isolated power (iso) function is currently defined as
function (slg, avg) 
{
    iso <- slg - avg
    return(iso)
  }
  
## Let's take 2014's MLB MVP, Mike Trout, and find his Isolated Power
## Stats for Mike Trout available on
## http://www.baseball-reference.com/players/t/troutmi01-bat.shtml
## For 2014, Trout had a SLG of .561 and an AVG of .287
## The formula for his Isolated Power using the iso function is below
## Output should be .274
iso(0.561,0.287)
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Slugging Percentage \link{slg}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ isolated power }