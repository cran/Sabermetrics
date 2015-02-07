\name{rcBasic}
\alias{rcBasic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Runs Created (Basic)}
\description{
Basic description of how many runs a hitter contributes to his team}
\usage{
rcBasic(H, BB, TB, AB)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{H}{
Hits}
  \item{BB}{
Walks}
  \item{TB}{
Total Bases}
  \item{AB}{
At Bats}
}
\value{
Returns ((H+BB)*TB)/(AB+BB)
}
\references{
\cite{http://en.wikipedia.org/wiki/Runs_created}}
\author{
Peter Xenopoulos }

\examples{
## This is a generic runs created formula
## Let's see how many runs created (keep in mind this is an estimate) ## a batter will make with 
## 100 hits, 7 walks (BB), 80 total bases, and 300 at bats

function (H, BB, TB, AB) 
{
    rc <- ((H + BB) * TB)/(AB + BB)
    return(rc)
  }
  
  rcBasic(100,7,80,300) # Should output 27.88274 runs
}

\seealso{
Runs Created (with stolen bases) \link{rcBasicSB} and Runs Created (Technical) \link{rcTech}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ runs created }