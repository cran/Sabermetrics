\name{pyth}
\alias{pyth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pythagorean Expectation}
\description{
Pythagorean expectation is a formula invented by Bill James to estimate how many games a baseball team "should" have won based on the number of runs they scored and allowed.}
\usage{
pyth(RS, RA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RS}{
Runs Scored}
  \item{RA}{
Runs Allowed}
}
\value{
Returns (RS*RS)/((RS*RS)+(RA*RA))
}
\references{
\cite{http://en.wikipedia.org/wiki/Pythagorean_expectation}}
\author{
Peter Xenopoulos }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ pythagorean expectation }