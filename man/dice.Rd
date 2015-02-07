\name{dice}
\alias{dice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Defense-Independent Component ERA (DICE)}
\description{
A function gives a number that is better at predicting a pitcher's ERA in the following year than the pitcher's actual ERA in the current year.}
\usage{
dice(HR, BB, HBP, K, IP)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{HR}{
Home Runs Allowed}
\item{BB}{
Walks Allowed}
\item{HBP}{
Batters Hit}
\item{K}{
Strikeouts}
\item{IP}{
Innings Pitched}
}
\value{
Returns 3 + ((13*HR+3*BB+3*HBP-2*K)/IP)
}
\references{
\cite{http://en.wikipedia.org/wiki/Defense_independent_pitching_statistics} }
\author{
Peter Xenopoulos }

\examples{
## Defense-Independent Component ERA (dice) function is currently defined as
function (HR, BB, HBP, K, IP) 
{
    defenseERA <- 3 + ((13 * HR + 3 * BB + 3 * HBP - 2 * K)/IP)
    return(defenseERA)
  }
  
## Let's take 2014's MLB MVP, Clayton Kershaw, and find his DICE
## Stats for Clayton Kershaw available on
## http://www.baseball-reference.com/players/k/kershcl01-pitch.shtml
## For 2014, Kershaw allowed 9 HR, 31 BB, 2 HBP, 239 K, and 198.1 IP
## The formula for his DICE using the dice function is below
## Output should be 1.677436
dice(9,31,2,239,198.1)
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dips }
\keyword{ dice }% __ONLY ONE__ keyword per line
