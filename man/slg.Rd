\name{slg}
\alias{slg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Slugging Percentage
}
\description{
Function to calculate the slugging percentage (total hitting power) of a player/team
}
\usage{
slg(TB, AB)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TB}{
Total bases
}
  \item{AB}{
At bats}
}
\details{
Slugging percentage is a popular measure to determine the hitting power of an entity}
\value{
Returns (TB/AB)
}
\examples{
## The slugging percentage (slg) function is currently defined as

function (TB, AB) 
{
    slugging <- (TB/AB)
    return(slugging)
  }
  
## Let's take 2014's MLB MVP, Mike Trout, and find his slugging percentage
## Stats for Mike Trout available on
## http://www.baseball-reference.com/players/t/troutmi01-bat.shtml
## For 2014, Trout had 602 AB, and 338 TB
## The formula for his slugging percentage using the slg function is below
## Output should be 0.5614618
slg(338, 602)
}


\references{
\cite{http://en.wikipedia.org/wiki/Slugging_percentage}}
\author{
Peter Xenopoulos 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ slugging percentage }

