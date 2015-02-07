\name{rcPX}
\alias{rcPX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Runs Created (PX Model)
}
\description{
Runs created model using linear weights from 2012, 2013 and 2014 MLB league data}
\usage{
rcPX(SINGLES, DOUBLES, TRIPLES, HR, BB, SB)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SINGLES}{
Number of singles
}
\item{DOUBLES}{
Number of doubles
}
\item{TRIPLES}{
Number of triples
}
\item{HR}{
Number of home runs
}
\item{BB}{
Number of unintentional walks
}
\item{SB}{
Number of stolen bases
}
}
\value{
Returns -391.39753 + 0.44953*(SINGLES) + 0.85285*(DOUBLES) + 1.05912*(TRIPLES) + 1.36359*(HR) + 0.31761*(BB) + 0.21599*(SB)
}
\references{
\cite{http://peterxeno.com/linear-weights-in-baseball-sabermetrics/}}
\author{
Peter Xenopoulos
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{rcBasic}, \link{rcBasicSB}, \link{rcTech}}
\examples{
## Let's say the LA Dodgers had 
## 952 singles, 302 doubles, 38 triples, 134 home runs, 519 walks, 
## 138 stolen bases

  rcPX(952,302,38,134,519,138) # Outputs 711.7296
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ runs created }
