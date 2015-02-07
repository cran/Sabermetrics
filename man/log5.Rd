\name{log5}
\alias{log5}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log5 Sabermetric formula}
\description{
Log 5 is a formula invented by Bill James to estimate the probability that team A will win a game, based on the true winning percentage of Team A and Team B. It's equivalent to the Bradley-Terry-Luce model used for paired comparisons, the Elo rating system used in chess and the Rasch model used in the analysis of categorical data.}
\usage{
log5(probA, probB, order)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{probA}{
Win probability of team A}
  \item{probB}{
Win probability of team B}
  \item{order}{
Determine winning probability of which team. 0 means win probability of A over B, and 1 vice-versa}
}
\value{
Returns (probA - (probA*probB)) / (probA + probB - (2 * probA * probB))
}
\references{
\cite{http://en.wikipedia.org/wiki/Log5}}
\author{
Peter Xenopoulos }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ log5 }