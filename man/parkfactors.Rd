\name{parkfactors}
\alias{parkfactors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Park Factors}
\description{
A function used to generate the park factors for a given season/team}
\usage{
parkfactors(year, team)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
Season}
  \item{team}{
Team}
}
\details{
0 = All Teams \cr
1 = Los Angeles Angels \cr
2 = Orioles \cr
3 = Boston Red Sox \cr
4 = Chicago White Sox \cr
5 = Cleveland Indians \cr
6 = Detroit Tigers \cr
7 = Kansas City Royals \cr
8 = Minnesota Twins \cr
9 = New York Yankees \cr
10 = Oakland Athletics \cr
11 = Seattle Mariners \cr
12 = Tampa Bay Rays \cr
13 = Texas Rangers \cr
14 = Toronto Blue Jays \cr
15 = Arizona Diamondbacks \cr
16 = Atlanta Braves \cr
17 = Chicago Cubs \cr
18 = Cincinnati Reds \cr
19 = Colorado Rockies \cr
20 = Florida Marlins \cr
21 = Houston Astros \cr
22 = Los Angeles Dodgers \cr
23 = Milwaukee Brewers \cr
24 = Washington Nationals \cr
25 = New York Mets \cr
26 = Philadelphia Phillies \cr
27 = Pittsburgh Pirates \cr
28 = St. Louis Cardinals \cr
29 = San Diego Padres \cr
30 = San Francisco Giants}
\value{
Returns a data frame with the respective park factors
}
\references{
http://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=2014}
\author{
Peter Xenopoulos}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{handedparkfactors}}
\examples{
## To get all of 2014 Park Factors
# parkfactors(2014)

## To get all of the Angels' Park Factors
# parkfactors(2014,1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ park factors }
