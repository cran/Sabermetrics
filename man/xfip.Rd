\name{xfip}
\alias{xfip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
xFIP
}
\description{
xFIP is a statistic that estimates a pitcher's expected run prevention independent of the performance of the defense.}
\usage{
xfip(flyballs, lgHRFB, BB, HBP, K, ip, year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flyballs}{
Fly balls allowed}
  \item{lgHRFB}{
League HR/FB Percentage}
  \item{BB}{
Balls Allowed}
  \item{HBP}{
Hit By Pitches Allowed}
  \item{K}{
Strikeouts}
  \item{ip}{
Innings Pitched}
  \item{year}{
Season}
}
\details{
xFIP is almost exactly like FIP, except it uses the expected number of home runs taking the pitcher's fly balls allowed times the league average home run per fly ball rate.}
\value{
Returns a numeric value equal to (13*(flyballs*lgHRFB)+(3*(BB+HBP))-(2*K))/(ip)+constant
}
\references{
http://www.fangraphs.com/library/pitching/xfip/}
\author{
Peter Xenopoulos}
\note{
Innings Pitched  (IP) is commonly reported with .1 indicating 1/3 of an inning and .2 indicating 2/3 of an inning. In order for this function to be accurate, please change the decimal to .333 for 1/3 of an inning and .666 for 2/3 of an inning.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{fip},\link{xfipminus}}
\examples{
## The function is currently defined as
function (flyballs, lgHRFB, BB, HBP, K, ip, year) 
{
    constant <- linearWeights$cFIP[which(linearWeights$Season == 
        year)]
    xfip <- (13 * (flyballs * lgHRFB) + (3 * (BB + HBP)) - (2 * 
        K))/(ip) + constant
    return(xfip)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ xfip }
\keyword{ expected fip }% __ONLY ONE__ keyword per line
