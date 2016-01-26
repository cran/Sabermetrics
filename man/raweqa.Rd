\name{raweqa}
\alias{raweqa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Raw Equivalent Average}
\description{
Raw Equivalent Average is used to calculate Equivalent Average}
\usage{
raweqa(AB, H, TB, BB, IBB, HBP, CS, SB, SH, SF)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{AB}{
At Bats}
  \item{H}{
Hits}
  \item{TB}{
Total Bases}
  \item{BB}{
Unintentional Walks}
  \item{IBB}{
Intentional Walks}
  \item{HBP}{
Hit By Pitches}
  \item{CS}{
Caught Stealing}
  \item{SB}{
Stolen Bases}
  \item{SH}{
Sacrifice Hits}
  \item{SF}{
Sacrifice Flies}
}
\value{
Returns a numerical value equal to (H+TB+1.5*(BB+HBP+SB)+SH+SF-IBB/2)/(AB+BB+HBP+SH+SF+CS+SB)
}
\references{
https://www.baseballprospectus.com/glossary/index.php?mode=viewstat&stat=61}
\author{
Peter Xenopoulos}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{ops},\link{slg},\link{obp},\link{iso},\link{babip}}
\examples{

## The function is currently defined as
function (AB, H, TB, BB, IBB, HBP, CS, SB, SH, SF) 
{
    raweqa <- (H + TB + 1.5 * (BB + HBP + SB) + SH + SF - IBB/2)/(AB + 
        BB + HBP + SH + SF + CS + SB)
    return(raweqa)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ raw eqa }
\keyword{ eqa }% __ONLY ONE__ keyword per line
