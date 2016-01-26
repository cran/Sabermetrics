\name{eraminus}
\alias{eraminus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ERA-}
\description{
ERA- is a park-adjusted version of ERA}
\usage{
eraminus(ERA, ParkFactor, LeagueERA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ERA}{
Player ERA}
  \item{ParkFactor}{
Park Factor}
  \item{LeagueERA}{
League Average ERA}
}
\details{
League Average is set to 100 each season. Each point below or above 100 is one percentage point better or worse than league average.}
\value{
Returns a value equal to (ERA+(ERA-(ERA*(ParkFactor/100))))/(LeagueERA)*100
}
\references{
http://www.fangraphs.com/library/pitching/era-fip-xfip/
http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=2014
}
\author{
Peter Xenopoulos}
\note{
Park Factors can be found in the references section}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{era},\link{fipminus},\link{xfipminus}}
\examples{
## The function is currently defined as
function (ERA, ParkFactor, LeagueERA) 
{
    eraminus <- (ERA + (ERA - (ERA * (ParkFactor/100))))/(LeagueERA) * 
        100
    return(eraminus)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ era- }
\keyword{ era minus }% __ONLY ONE__ keyword per line
