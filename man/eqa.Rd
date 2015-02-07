\name{eqa}
\alias{eqa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Equivalent Average}
\description{
A baseball metric invented by Clay Davenport and intended to express the production of hitters in a context independent of park and league effects. EQA represents a hitter's productivity using the same scale as batting average.}
\usage{
eqa(H, TB, BB, HBP, SB, SAC, SF, AB, CS)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{H}{
Hits}
  \item{TB}{
Total Bases}
  \item{BB}{
Walks}
  \item{HBP}{
Hit by pitch}
  \item{SB}{
Stolen bases}
  \item{SAC}{
Sacrifice hit/bunt}
  \item{SF}{
Sacrifice flies}
  \item{AB}{
At bats}
  \item{CS}{
Caught stealing}
}
\value{
Returns (H+TB+1.5*(BB+HBP)+SB+SAC+SF)/(AB+BB+HBP+SAC+SF+CS+(SB/3))
}
\references{
\cite{http://en.wikipedia.org/wiki/Equivalent_average}}
\author{
Peter Xenopoulos }

\examples{
## The equivalent average (eqa) function is currently defined as
function (H, TB, BB, HBP, SB, SAC, SF, AB, CS) 
{
    eqa <- (H + TB + 1.5 * (BB + HBP) + SB + SAC + SF)/(AB + 
        BB + HBP + SAC + SF + CS + (SB/3))
    return(eqa)
  }
  
## Let's take 2014's MLB MVP, Mike Trout, and find his OPS
## Stats for Mike Trout available on
## http://www.baseball-reference.com/players/t/troutmi01-bat.shtml
## For 2014, Trout had 173 H, 338 TB, 83 BB, 10 HBP, 16 SB, 0 SAC, 10 SF, 602 AB, 2 CS
## The formula for his EQA using the ops function is below
## Output should be .9496958
eqa(173,338,83,10,16,0,10,602,2)
}
%% ~Make other sections like Warning with \section{Warning }{....} ~


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ equivalent average }
