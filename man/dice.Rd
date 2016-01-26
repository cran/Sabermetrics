\name{dice}
\alias{dice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Defense-Independent Component ERA}
\description{
DICE represents a way to measure a pitcher's performance on the ERA scale using only events a pitcher can "control"}
\usage{
dice(hr, bb, hbp, k, ip)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hr}{
Home Runs}
  \item{bb}{
Walks}
  \item{hbp}{
Hit By Pitches}
  \item{k}{
Strikeouts}
  \item{ip}{
Innings Pitched}
}
\details{
DICE uses the typical ERA scale.}
\value{
Returns a value equal to (13*hr+3*bb+3*hbp-2*k)/ip + 3.0
}
\references{
https://en.wikipedia.org/wiki/Defense-Independent_Component_ERA}
\author{
Peter Xenopoulos
}
\note{
Innings Pitched  (IP) is commonly reported with .1 indicating 1/3 of an inning and .2 indicating 2/3 of an inning. In order for this function to be accurate, please change the decimal to .333 for 1/3 of an inning and .666 for 2/3 of an inning.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{era},\link{fip},\link{xfip},\link{whip}}
\examples{

## The function is currently defined as
function (hr, bb, hbp, k, ip) 
{
    dice = (13 * hr + 3 * bb + 3 * hbp - 2 * k)/ip + 3
    return(dice)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dice }
\keyword{ defense independent pitching }% __ONLY ONE__ keyword per line
