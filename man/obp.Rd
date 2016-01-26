\name{obp}
\alias{obp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
On-Base Percentage
}
\description{
The OBP function calculates how often a player gets on base.
}
\usage{
obp(H, BB, HBP, AB, SF)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{H}{
Number of Hits
}
  \item{BB}{
Number of bases on balls (walks)
}
  \item{HBP}{
Number of hit by pitches
}
  \item{AB}{
Number of at bats}
  \item{SF}{
Number of sacrifice flies
}
}
\value{
Returns a numerical value equal to ((H+BB+HBP)/(AB+BB+SF+HBP))
}
\references{
https://en.wikipedia.org/wiki/On-base_percentage
}
\author{
Peter Xenopoulos
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{ops}
}
\examples{
## Let's calculate Mike Trout's OBP for the 2014 season
## He had 173 hits, 83 bases on balls, 10 hit by pitches, 602 at bats, and 10 sacrifice flies
## We should get 0.377 as our output
obp(173,83,10,602,10)

## The function is currently defined as
function (H, BB, HBP, AB, SF) 
{
    onbase <- ((H + BB + HBP)/(AB + BB + SF + HBP))
    return(onbase)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{obp}
\keyword{on base percentage}% __ONLY ONE__ keyword per line
