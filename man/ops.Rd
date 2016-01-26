\name{ops}
\alias{ops}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
On-Base Plus Slugging}
\description{
A metric that protrays a hitter's ability to get on base and to hit for power.}
\usage{
ops(slg, obp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{slg}{
Slugging Percentage}
  \item{obp}{
On-Base Percentage}
}
\value{
Returns a numerical value equal to (OBP)+(SLG)
}
\references{
http://www.fangraphs.com/library/offense/ops/
https://en.wikipedia.org/wiki/On-base_plus_slugging
}
\author{
Peter Xenopoulos
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{opsplus},
\link{obp}
}
\examples{
## Let's calculate Mike Trout's OPS for the 2014 season
## He had an OBP = .377 and a SLG = .561
## We should get .938 as our output
ops(.561,.377)

## The function is currently defined as
function (slg, obp) 
{
    ops <- slg + obp
    return(ops)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ on base plus slugging }
\keyword{ ops }% __ONLY ONE__ keyword per line
