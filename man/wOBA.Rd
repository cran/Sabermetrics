\name{wOBA}
\alias{wOBA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted On Base Average}
\description{
Finds the weighted on-base average, a statistic based on linear weights of events}
\usage{
wOBA(BB, HBP, SINGLE, RBOE, DOUBLE, TRIPLE, HR, PA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BB}{
Walks}
  \item{HBP}{
Hit by pitch}
  \item{SINGLE}{
Number of singles}
  \item{RBOE}{
Number of bases reached on error}
  \item{DOUBLE}{
Number of doubles}
  \item{TRIPLE}{
Number of triple}
  \item{HR}{
Number of home runs}
  \item{PA}{
Number of plate appearances}
}
\value{
Returns ((0.72*BB)+(0.75*HBP)+(0.90*SINGLE)+(0.92*RBOE)+(1.24*DOUBLE)+(1.56*TRIPLE)+(1.95*HR))/PA which is based on linear weights
}
\references{
\cite{http://en.wikipedia.org/wiki/WOBA}}
\author{
Peter Xenopoulos }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ wOBA }