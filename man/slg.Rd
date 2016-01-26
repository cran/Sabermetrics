\name{slg}
\alias{slg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Slugging Percentage}
\description{
Slugging percentage is a measure of a hitter's power.}
\usage{
slg(TB, AB)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TB}{
Total Bases
}
  \item{AB}{
At Bats}
}
\value{
Returns a numerical value equal to (TB/AB)

This is also equivalent to ((singles) + (2 x doubles) + (3 x triples) + (4 x homeruns))/(AB)
}
\references{
https://en.wikipedia.org/wiki/Slugging_percentage}
\author{
Peter Xenopoulos}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{opsplus},
\link{ops}}
\examples{
## Let's calculate Mike Trout's SLG for the 2014 season
## He had 338 TB's and 602 AB's
## We should get .561 as our output
slg(338,602)

## The function is currently defined as
function (TB, AB) 
{
    slugging <- (TB/AB)
    return(slugging)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ slugging percentage }
\keyword{ slg }% __ONLY ONE__ keyword per line
