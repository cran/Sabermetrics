\name{iso}
\alias{iso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Isolated Power
}
\description{
Isolated power measures a hitter's ability to hit for extra bases.
}
\usage{
iso(slg, avg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{slg}{
Slugging Percentage}
  \item{avg}{
Batting Average}
}
\value{
Returns a numerical value equal to SLG-AVG
}
\references{
http://www.fangraphs.com/library/offense/iso/}
\author{
Peter Xenopoulos
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{opsplus},
\link{ops}
}
\examples{
## Let's calculate Mike Trout's isolated power in 2014
## He had a SLG = .561 and an AVG = .287
## We should get .274
iso(.561,.287)

## The function is currently defined as
function (slg, avg) 
{
    iso <- slg - avg
    return(iso)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ iso }
\keyword{ isolated power }% __ONLY ONE__ keyword per line
