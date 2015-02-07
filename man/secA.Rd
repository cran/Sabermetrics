\name{secA}
\alias{secA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Secondary Average}
\description{
Secondary average, or SecA, is a baseball statistic that measures the sum of extra bases gained on hits, walks, and stolen bases (less times caught stealing) depicted per at bat.}
\usage{
secA(BB, TB, H, SB, CS, AB)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BB}{
Walks}
  \item{TB}{
Total Bases}
  \item{H}{
Hits}
  \item{SB}{
Stolen Bases}
  \item{CS}{
Caught Stealing}
  \item{AB}{
At Bats}
}
\value{
Returns (BB+(TB-H)+(SB-CS))/AB
}
\references{
\cite{http://en.wikipedia.org/wiki/Secondary_average}}
\author{
Peter Xenopoulos }
\examples{
## The secondary average (secA) function is currently defined as
function (BB, TB, H, SB, CS, AB) 
{
    avg <- (BB + (TB - H) + (SB - CS))/AB
    return(avg)
  }
  
## Let's take 2014's MLB MVP, Mike Trout, and find his SecA
## Stats for Mike Trout available on
## http://www.baseball-reference.com/players/t/troutmi01-bat.shtml
## For 2014, Trout had 83 BB, 338 TB, 173 H, 16 SB, 2 CS, 602 AB
## The formula for his SecA using the ops function is below
## Output should be .4352159
secA(83,338,173,16,2,602)
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ secA }
\keyword{ secondary average }% __ONLY ONE__ keyword per line
