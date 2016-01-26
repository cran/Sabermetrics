\name{log5}
\alias{log5}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log5}
\description{
Log5 is a way to estimate the probability that Team A will win a game given the true winning probabilities of Team A and Team B}
\usage{
log5(pA, pB, order = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pA}{
Probability that A wins}
  \item{pB}{
Probability that B wins}
  \item{order}{
0 = A over B and 1 = B over A}
}
\value{
Returns a value equal to (pA-pA*pB)/(pA+pB-2*pA*pB)
}
\references{
https://en.wikipedia.org/wiki/Log5}
\author{
Fernando Crema, Peter Xenopoulos}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{pyth}}
\examples{
## Let's assume team A has a .60 true probability of winning
## Let's assume team B has a .40 true probability of winning
## We should get an output of 0.6923
log5(.60,.40)

## The function is currently defined as
function (pA, pB, order = 0) 
{
    if (order) {
        aux = pB
        pB = pA
        pA = aux
    }
    log5 <- (pA - pA * pB)/(pA + pB - 2 * pA * pB)
    return(log5)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
