\name{lgwSB}
\alias{lgwSB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
League wSB}
\description{
This function helps find the League wSB}
\usage{
lgwSB(year, SB, CS, single, bb, hbp, ibb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
Season}
  \item{SB}{
League Stolen Bases}
  \item{CS}{
League Caught Stealing}
  \item{single}{
Singles}
  \item{bb}{
Walks}
  \item{hbp}{
Hit By Pitches}
  \item{ibb}{
Intentional Walks}
}
\value{
Returns a value equal to (SB * runSB + CS * runCS) / (single + bb + hbp - ibb)
}
\references{
http://www.fangraphs.com/library/offense/wsb/}
\author{
Peter Xenopoulos}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{wsb}}
\examples{
## The function is currently defined as
function (year, SB, CS, single, bb, hbp, ibb) 
{
    runSB <- linearWeights$runSB[which(linearWeights$Season == 
        year)]
    runCS <- linearWeights$runCS[which(linearWeights$Season == 
        year)]
    lgwSB <- (SB * runSB + CS * runCS)/(single + bb + hbp - ibb)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ league wsb }
