\name{rcTech}
\alias{rcTech}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Runs Created (Technical)}
\description{
How many runs a hitter contributes to his team (technical version)}
\usage{
rcTech(H, BB, CS, HBP, GIDP, TB, IBB, SAC, SF, SB)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{H}{
Hits}
  \item{BB}{
Unintentional walks}
  \item{CS}{
Caught stealing}
  \item{HBP}{
Hit by pitch}
  \item{GIDP}{
Grounded into double play}
  \item{TB}{
Total bases}
  \item{IBB}{
Intentional walks}
  \item{SAC}{
Sacrifice hits/bunts}
  \item{SF}{
Sacrifice flies}
  \item{SB}{
Stolen bases}
}
\value{
Returns (H+BB-CS+HBP-GIDP)*(TB + (0.26*(BB-IBB+HBP)) + (0.52*(SAC+SF+SB)))
}
\references{
\cite{http://en.wikipedia.org/wiki/Runs_created}}
\author{
Peter Xenopoulos }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Runs Created (Basic) \link{rcBasic} and Runs Created (with Stolen Bases) \link{rcBasicSB}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ runs created }