\name{fip}
\alias{fip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Field Independent Pitching}
\description{
Similar to DICE \link{dice}}
\usage{
fip(HR, BB, K, IP, C)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{HR}{
Home Runs allowed}
  \item{BB}{
Walks}
  \item{K}{
Strikeouts}
  \item{IP}{
Innings Pitched}
  \item{C}{
League average ERA}
}
\value{
Returns ((13*HR+3*BB-2*K)/IP) + C
}
\references{
\cite{http://en.wikipedia.org/wiki/Defense_independent_pitching_statistics}}
\author{
Peter Xenopoulos }
%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
## Field Independent Pitching (fip) function is currently defined as
function (HR, BB, K, IP, C) 
{
    fieldIndPitch <- ((13 * HR + 3 * BB - 2 * K)/IP) + C
    return(fieldIndPitch)
  }
  
## Let's take 2014's MLB MVP, Clayton Kershaw, and find his FIPS
## Stats for Clayton Kershaw available on
## http://www.baseball-reference.com/players/k/kershcl01-pitch.shtml
## For 2014, Kershaw allowed 9 HR, 31 BB, 239 K, 198.1 IP and league era (C) of 3.66
## The formula for his FIPS using the dice function is below
## Output should be 2.307148
fip(9,31,239,198.1,3.66)
}


\seealso{
DICE \link{dice}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dips }
\keyword{ fip }% __ONLY ONE__ keyword per line
