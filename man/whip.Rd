\name{whip}
\alias{whip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
WHIP
}
\description{
WHIP (Walks + Hits Per Innings Pitched) describes how many base runners a pitcher gives up an inning.}
\usage{
whip(h, bb, ip)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{
Hits}
  \item{bb}{
Walks}
  \item{ip}{
Innings Pitched}
}
\details{
WHIP is not a perfect indicator of pitcher skill. It is highly defense dependent. Hit rates are largely outside of pitcher control and have high fluctuations.}
\value{
Returns a numerical value equal to (h+bb)/ip
}
\references{
http://www.fangraphs.com/library/pitching/whip/

https://en.wikipedia.org/wiki/Walks_plus_hits_per_inning_pitched}
\author{
Fernando Crema, Peter Xenopoulos}
\note{
Innings Pitched  (IP) is commonly reported with .1 indicating 1/3 of an inning and .2 indicating 2/3 of an inning. In order for this function to be accurate, please change the decimal to .333 for 1/3 of an inning and .666 for 2/3 of an inning.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{era},\link{fip},\link{dice},\link{xfip}}
\examples{
## Let's look at Clayton Kershaw's WHIP for the 2014 season
## He gave up H = 139 and BB = 31 and pitched 198.33 innings
## We should get an output of 0.857
whip(139,31,198.33)

## The function is currently defined as
function (h, bb, ip) 
{
    whip <- (h + bb)/ip
    return(whip)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ whip }
\keyword{ walks plus hits per inning pitched }% __ONLY ONE__ keyword per line
