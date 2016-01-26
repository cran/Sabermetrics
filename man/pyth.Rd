\name{pyth}
\alias{pyth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pythagorean Expectation}
\description{
Pythagorean expectation portrays what a team's winning percentage "should" be given it's runs scored and allowed.}
\usage{
pyth(rs, ra, alpha = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rs}{
Runs Scored}
  \item{ra}{
Runs Allowed}
  \item{alpha}{
Exponent to use}
}
\details{
2 and 1.83 are the most common exponents used. 1.83 is the exponent baseball-reference uses in its calculations.}
\value{
Returns a value equal to (rs^alpha)/(rs^alpha + ra^alpha)
}
\references{
http://www.fangraphs.com/library/principles/expected-wins-and-losses/
https://en.wikipedia.org/wiki/Pythagorean_expectation}
\author{
Peter Xenopoulos}
\note{
Pythagorean expectation can be used in a multitude of sports like basketball and football
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{log5}}
\examples{
## Let's calculate the Angels 2014 Pythagorean Expectation
## The Angels had 773 Runs Scored and 630 Runs Allowed
## We should get an output close to .600
pyth(773,630,2)

## The function is currently defined as
function (rs, ra, alpha = 2) 
{
    pyth <- (rs^alpha)/(rs^alpha + ra^alpha)
    return(pyth)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ pythagorean }
\keyword{ pyth }% __ONLY ONE__ keyword per line
