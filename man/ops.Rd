\name{ops}
\alias{ops}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
On-base plus Slugging}
\description{
Function to calculate on base percentage plus slugging percentage. This is a measure of a hitter's ability to hit for power and get on base.}
\usage{
ops(slg, obp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{slg}{
Slugging percentage. Found from \link{slg}
}
  \item{obp}{
On-base percentage. Found from \link{obp}}
}

\value{
Returns On-Base Percentage + Slugging Percentage
}
\references{
\cite{http://en.wikipedia.org/wiki/On-base_plus_slugging}
}
\author{
Peter Xenopoulos }

\examples{
## The on-base percentage plus slugging (ops) function is currently defined as
function (slg, obp) 
{
    ops <- slg + obp
    return(ops)
  }
  
## Let's take 2014's MLB MVP, Mike Trout, and find his OPS
## Stats for Mike Trout available on
## http://www.baseball-reference.com/players/t/troutmi01-bat.shtml
## For 2014, Trout had a SLG of .561 and an OBP of .377
## The formula for his OPS using the ops function is below
## Output should be .938
ops(0.561,0.377)
}

\seealso{
On-base Percentage \link{obp} and Slugging Percentage \link{slg}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ops }
\keyword{ on base plus slugging }% __ONLY ONE__ keyword per line
