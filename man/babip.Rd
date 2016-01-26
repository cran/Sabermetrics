\name{babip}
\alias{babip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Batting Average on Balls in Play (BABIP)}
\description{
BABIP is a statistic which measures how often a non-home run batted ball falls for a hit.}
\usage{
babip(h, hr, ab, k, sf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{
Hits}
  \item{hr}{
Home Runs}
  \item{ab}{
At Bats}
  \item{k}{
Strikeouts}
  \item{sf}{
Sacrifice Flies}
}
\value{
Returns a numerical value equal to (h-hr)/(ab-hr-k-sf)
}
\references{
http://www.fangraphs.com/library/pitching/babip/}
\author{
Fernando Crema, Peter Xenopoulos}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{obp},\link{ops}}
\examples{
## Let's Calculate Mike Trout's BABIP for the 2014 season
## He had 173 Hits, 36 HR's, 602 AB's, 184 K's, and 10 SF's
## We should get .368 as our output
babip(173,36,602,184,10)

## The function is currently defined as
function (h, hr, ab, k, sf) 
{
    babip <- (h-hr)/(ab-hr-k-sf)
    return(babip)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ babip }
\keyword{ batting average }% __ONLY ONE__ keyword per line
