\name{secavg}
\alias{secavg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Secondary Average}
\description{
Secondary average measure the sum of extra bases gained on hits, walks and stolen bases.}
\usage{
secavg(ab, bb, tb, h, sb, cs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ab}{
At Bats}
  \item{bb}{
Walks}
  \item{tb}{
Total Bases}
  \item{h}{
Hits}
  \item{sb}{
Stolen Bases}
  \item{cs}{
Caught Stealing}
}
\details{
SecAvg shares no correlation with batting average although overall league averages are inclined to correspond with league batting averages.}
\value{
Returns a numerical value equal to (bb+tb-h+sb-cs)/ab
}
\references{
https://en.wikipedia.org/wiki/Secondary_average}
\author{
Peter Xenopoulos}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{ops},\link{slg},\link{iso}}
\examples{

## The function is currently defined as
function (ab, bb, tb, h, sb, cs) 
{
    SecA <- (bb + tb - h + sb - cs)/ab
    return(SecA)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ secondary average }
\keyword{ secavg }% __ONLY ONE__ keyword per line
