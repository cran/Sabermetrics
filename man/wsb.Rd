\name{wsb}
\alias{wsb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
wSB}
\description{
wSB is the estimated number of runs a player contributes by stealing bases as compared to the average player.}
\usage{
wsb(year, SB, CS, lgwSB, single, bb, hbp, ibb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
Season}
  \item{SB}{
Stolen Bases}
  \item{CS}{
Caught Stealing}
  \item{lgwSB}{
League wSB}
  \item{single}{
Singles}
  \item{bb}{
Walks}
  \item{hbp}{
Hit By Pitches}
  \item{ibb}{
Intentional Walks}
}
\details{
wSB is useful because it attaches a value to a player's SB count}
\value{
Returns a value equal to (SB * runSB) + (CS * runCS) - (lgwSB * (single + bb + hbp - ibb))
}
\references{
http://www.fangraphs.com/library/offense/wsb/}
\author{
Peter Xenopoulos}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{lgwSB}}
\examples{
## The function is currently defined as
function (year, SB, CS, lgwSB, single, bb, hbp, ibb) 
{
    runSB <- linearWeights$runSB[which(linearWeights$Season == 
        year)]
    runCS <- linearWeights$runCS[which(linearWeights$Season == 
        year)]
    wsb <- (SB * runSB) + (CS * runCS) - (lgwSB * (single + bb + 
        hbp - ibb))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ stolen bases }
\keyword{ wSB }% __ONLY ONE__ keyword per line
