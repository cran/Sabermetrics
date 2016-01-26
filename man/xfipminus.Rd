\name{xfipminus}
\alias{xfipminus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
xFIP-}
\description{
xFIP- is a park-adjusted version of xFIP}
\usage{
xfipminus(xFIP, ParkFactor, LeaguexFIP)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xFIP}{
Player xFIP}
  \item{ParkFactor}{
Park Factor}
  \item{LeaguexFIP}{
League Average xFIP}
}
\details{
League Average is set to 100 each season. Each point below or above 100 is one percentage point better or worse than league average.}
\value{
Returns a value equal to (xFIP+(xFIP-(xFIP*(ParkFactor/100))))/(LeaguexFIP)*100
}
\references{
http://www.fangraphs.com/library/pitching/era-fip-xfip/
http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=2014}
\author{
Peter Xenopoulos}
\note{
Park Factors can be found in the references section}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{xfip},\link{fipminus},\link{eraminus}}
\examples{
## The function is currently defined as
function (xFIP, ParkFactor, LeaguexFIP) 
{
    xfipminus <- (xFIP + (xFIP - (xFIP * (ParkFactor/100))))/(LeaguexFIP) * 
        100
    return(xfipminus)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ xfip- }
\keyword{ xfip minus }% __ONLY ONE__ keyword per line
