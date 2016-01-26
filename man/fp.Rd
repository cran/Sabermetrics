\name{fp}
\alias{fp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fielding Percentage}
\description{
A measure that reflects the percentage of times a player successfully handles a batted or thrown ball.}
\usage{
fp(po, a, e)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{po}{
Putouts}
  \item{a}{
Assists}
  \item{e}{
Errors}
}
\details{
Fielding percentage is not a particularly sought after defensive metric, especially with the advent of more comprehensive metrics such as UZR and DRS.}
\value{
Returns a value equal to (po+a)/(po+a+e)
}
\references{
https://en.wikipedia.org/wiki/Fielding_percentage}
\author{
Fernando Crema, Peter Xenopoulos}
\note{
Fielding percentage is misleading in the fact that it does not account for a player's range -- that is, if a player that cannot get to a ball gives up a hit instead of having an opportunity to make an out or error.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
## Let's calculate Mike Trout's fielding percentage for 2014
## He had PO = 383, A = 4, E = 3
## We should get an output of .992
fp(383,4,3)

## The function is currently defined as
function (po, a, e) 
{
    fp <- (po + a)/(po + a + e)
    return(fp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ fielding percentage }
\keyword{ defense }% __ONLY ONE__ keyword per line
