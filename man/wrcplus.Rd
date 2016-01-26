\name{wrcplus}
\alias{wrcplus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
wRC+}
\description{
wRC+ is a statistic which attempts to credit a hitter for the value of each outcome.}
\usage{
wrcplus(wRAA, PA, year, parkfactor, leaguewRC)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wRAA}{
Weighted Runs Above Average (see \link{wraa})}
  \item{PA}{
Plate Appearances}
  \item{year}{
Season}
  \item{parkfactor}{
Park Factor}
  \item{leaguewRC}{
League wRC}
}
\details{
wRC+ is scaled so that league average is 100, and each point above or below 100 is 1 percentage point better or worse than league average}
\value{
Returns a value equal to (((wRAA/PA + leaguerpa)+(leaguerpa-parkfactor*leaguerpa))/leaguewRC)*100
}
\references{
http://www.fangraphs.com/library/offense/wrc/}
\author{
Peter Xenopoulos}
\note{
wRC+ is a commonly used offensive statistic}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{wraa},\link{woba},\link{wrc}}
\examples{
## The function is currently defined as
function (wRAA, PA, year, parkfactor, leaguewRC) 
{
    leaguerpa <- linearWeights$RPerPA[which(linearWeights$Season == 
        year)]
    wrcplus <- (((wRAA/PA + leaguerpa) + (leaguerpa - parkfactor * 
        leaguerpa))/leaguewRC) * 100
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ wrc+ }
\keyword{ wrc plus }% __ONLY ONE__ keyword per line
