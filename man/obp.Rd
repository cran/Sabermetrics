\name{obp}
\alias{obp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
On-Base Percentage
}
\description{
Function to calculate the on-base percentage of a player/team
}
\usage{
obp(H, BB, HBP, AB, SF)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{H}{
Hits
}
\item{BB}{
Unintentional Walks
}
\item{HBP}{
Hit by pitch
}
\item{AB}{
At bats
}
\item{SF}{
Sacrifice flies
}
}
\details{
On-base percentage is used to figure out how often an entity gets on-base
}
\value{
Returns the following: ((H+BB+HBP)/(AB+BB+SF+HBP))
}
\references{
\cite{http://en.wikipedia.org/wiki/On-base_percentage}
}
\author{
Peter Xenopoulos 
}
\examples{
## The on-base percentage (obp) function is currently defined as

function (H, BB, HBP, AB, SF) 
{
  onbase <- ((H+BB+HBP)/(AB+BB+SF+HBP))
  return(onbase)
}
  
## Let's take 2014's MLB MVP, Mike Trout, and find his on-base percentage
## Stats for Mike Trout available on
## http://www.baseball-reference.com/players/t/troutmi01-bat.shtml
## For 2014, Trout had 173 H, 83 BB, 10 HBP, 602 AB, 10 SF
## The formula for his on-base percentage using the obp function is below
## Output should be 0.377305
obp(173,83,10,602,10)
}
\seealso{
Slugging Percentage \link{slg}, OPS \link{ops} and Isolated Power \link{iso}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ on base percentage }