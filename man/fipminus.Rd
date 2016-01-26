\name{fipminus}
\alias{fipminus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FIP-}
\description{
FIP- is a park-adjusted version of FIP}
\usage{
fipminus(FIP, ParkFactor, LeagueFIP)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FIP}{
Player FIP}
  \item{ParkFactor}{
Park Factor}
  \item{LeagueFIP}{
League Average FIP}
}
\details{
League Average is set to 100 each season. Each point below or above 100 is one percentage point better or worse than league average.}
\value{
Returns a value equal to (FIP+(FIP-(FIP*(ParkFactor/100))))/(LeagueFIP)*100
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
\link{fip},\link{eraminus},\link{xfipminus}}
\examples{
## The function is currently defined as
function (FIP, ParkFactor, LeagueFIP) 
{
    fipminus <- (FIP + (FIP - (FIP * (ParkFactor/100))))/(LeagueFIP) * 
        100
    return(fipminus)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ fip- }
\keyword{ fip minus }% __ONLY ONE__ keyword per line
