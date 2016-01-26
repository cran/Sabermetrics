\name{woba}
\alias{woba}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted On-Base Average (wOBA)}
\description{
wOBA is a statistic which attempts to credit a batter for each outcome. It is reported as a rate.}
\usage{
woba(year, AB, BB, IBB, HBP, single, double, triple, HR, SF)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
Season to use as context}
  \item{AB}{
At Bats}
  \item{BB}{
Unintentional Walks}
  \item{IBB}{
Intentional Walks}
  \item{HBP}{
Hit By Pitches}
  \item{single}{
Singles}
  \item{double}{
Doubles}
  \item{triple}{
Triples}
  \item{HR}{
Home Runs}
  \item{SF}{
Sacrifice Flies}
}
\details{
The linear weights used for this equation are in the \link{linearWeights} dataframe.}
\value{
Returns a numeric value equal to ((wBB*BB)+(wHBP*HBP)+(w1B*single)+(w2B*double)+(w3B*triple)+(wHR*HR))/(AB+BB-IBB+SF+HBP) where wXX means weight of XX
}
\references{
http://www.fangraphs.com/library/offense/woba/
}
\author{
Peter Xenopoulos}
\note{
wOBA is regarded as a better representation of a hitter's skill than batting average or obp.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{obp},\link{wraa}}
\examples{
## Let's calculate Mike Trout's 2014 wOBA
## He had 602 AB's, 83 BB's, 6 IBB's, 10 HBP's, 
## 89 singles, 39 doubles, 9 triples, 36 homeruns, and 10 sac-flies
## We should get .408 as our output
woba(2014,602,83,6,10,89,39,9,36,10)

## The function is currently defined as
function (year, AB, BB, IBB, HBP, single, double, triple, HR, 
    SF) 
{
    wBB <- linearWeights$wBB[which(weights$Season == year)]
    wHBP <- linearWeights$wHBP[which(weights$Season == year)]
    w1B <- linearWeights$w1B[which(weights$Season == year)]
    w2B <- linearWeights$w2B[which(weights$Season == year)]
    w3B <- linearWeights$w3B[which(weights$Season == year)]
    wHR <- linearWeights$wHR[which(weights$Season == year)]
    woba <- ((wBB * BB) + (wHBP * HBP) + (w1B * single) + (w2B * 
        double) + (w3B * triple) + (wHR * HR))/(AB + BB - IBB + 
        SF + HBP)
    return(woba)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ woba }
\keyword{ weighted on base }% __ONLY ONE__ keyword per line
