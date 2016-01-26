\name{opsplus}
\alias{opsplus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
OPS+}
\description{
A useful metric to describe a hitter's ability in terms of the league.}
\usage{
opsplus(obp, slg, lgOBP, lgSLG)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obp}{
On-base percentage}
  \item{slg}{
Slugging percentage}
  \item{lgOBP}{
League OBP}
  \item{lgSLG}{
League Slugging Percentage}
}
\value{
Returns a numerical value equal to ((obp/lgOBP)+(slg/lgSLG)-1)*100

100 = League Average
90 = 10 percent BELOW League Average
110 = 10 percent ABOVE League Average
}
\references{
http://sports.yahoo.com/mlb/blog/big_league_stew/post/Everything-you-always-wanted-to-know-about-OPS-?urn=mlb,204667

http://www.fangraphs.com/library/offense/ops/
}
\author{
Peter Xenopoulos}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{ops},
\link{slg},
\link{obp}}
\examples{
## Let's calculate Mike Trout's 2014 OPS+
## He had an OBP = .377 and a SLG = .561
## League OBP = .316 and League SLG = .404 
## We should get 158.1652 as our output
opsplus(.377,.561,.316,.404)

## The function is currently defined as
function (obp, slg, lgOBP, lgSLG) 
{
    opsplus <- ((obp/lgOBP) + (slg/lgSLG) - 1) * 100
    return(opsplus)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ops+ }
\keyword{ ops plus }% __ONLY ONE__ keyword per line
