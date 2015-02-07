\name{rcBasicSB}
\alias{rcBasicSB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Runs Created (with Stolen Bases)}
\description{
Basic description of how many runs a hitter contributes to his team (with stolen bases included)}
\usage{
rcBasicSB(H, BB, TB, AB, CS, SB)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{H}{
Hits}
  \item{BB}{
Walks}
  \item{TB}{
Total Bases}
  \item{AB}{
At Bats}
\item{CS}{
Caught Stealing}
  \item{SB}{
Stolen Bases}
}
\value{
Returns ((H+BB-CS)*(TB+(0.55*SB)))/(AB+BB)
}
\references{
\cite{http://en.wikipedia.org/wiki/Runs_created}}
\author{
Peter Xenopoulos }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Runs Created (Basic) \link{rcBasic} and Runs Created (Technical) \link{rcTech}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ runs created }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
