\name{fip}
\alias{fip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Field Independent Pitching (FIP)}
\description{
FIP is a statistic that measure a pitcher's performance independent of defense. FIP uses outcomes that do not take into account a team's defense.}
\usage{
fip(HR, BB, HBP, K, IP, year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{HR}{
Homeruns given up}
  \item{BB}{
Walks given up}
  \item{HBP}{
Hit by pitches given up}
  \item{K}{
Strikeouts}
  \item{IP}{
Innings Pitched}
  \item{year}{
Season}
}
\details{
While FIP is not a complete representation of a pitcher's performance, it is regarded as a better representation of performance than ERA.}
\value{
Returns a numerical vector equal to ((13*HR)+(3*(BB+HBP))-(2*K))/IP + constant

Constant = Season FIP Constant
}
\references{
http://www.fangraphs.com/library/pitching/fip/}
\author{
Peter Xenopoulos}
\note{
Innings Pitched  (IP) is commonly reported with .1 indicating 1/3 of an inning and .2 indicating 2/3 of an inning. In order for this function to be accurate, please change the decimal to .333 for 1/3 of an inning and .666 for 2/3 of an inning.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{era},
\link{whip},
\link{xfip},
\link{fipminus}}
\examples{
## Let's calculate Clayton Kershaw's 2014 FIP
## He had 9 HR's, 31 BB's, 2 HBP's, 239 K's, and 198.333 IP's
## We should get 1.81 as our output
fip(9,31,2,239,198.333,2014)

## The function is currently defined as
function (HR, BB, HBP, K, IP, year) 
{
    constant <- weights$cFIP[which(weights$Season == year)]
    fip <- ((13 * HR) + (3 * (BB + HBP)) - (2 * K))/IP + constant
    return(fip)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ fip }
\keyword{ fielding independent }% __ONLY ONE__ keyword per line
