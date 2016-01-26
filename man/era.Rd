\name{era}
\alias{era}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Earned-Run Average (ERA)}
\description{
ERA is a basic metric used to describe a pitchers ability to prevent runs.}
\usage{
era(er, ip)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{er}{
Earned Runs}
  \item{ip}{
Innings Pitched}
}
\details{
ERA is not a perfect indicator of pitcher skill. It is highly defense dependent.}
\value{
Returns a numerical value equal to (9*er)/ip
}
\references{
http://www.fangraphs.com/library/pitching/era/}
\author{
Fernando Crema, Peter Xenopoulos}
\note{
Innings Pitched  (IP) is commonly reported with .1 indicating 1/3 of an inning and .2 indicating 2/3 of an inning. In order for this function to be accurate, please change the decimal to .333 for 1/3 of an inning and .666 for 2/3 of an inning.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{whip},\link{fip},\link{xfip}}
\examples{
## Let's calculate Clayton Kershaw's ERA for the 2014 Season
## He had 39 ER's and 198.333 IP
## We should get an ERA = to 1.77
era(39,198.333)

## The function is currently defined as
function (er, ip) 
{
    era <- (9 * er)/ip
    return(era)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ era }
\keyword{ earned run average }% __ONLY ONE__ keyword per line
