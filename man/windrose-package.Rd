\name{windrose-package}
\alias{windrose-package}
\alias{windrose}
\docType{package}
\title{
Provides manipulation and plotting of wind speed and direction data.
}
\description{
More about what it does (maybe more than one line)
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab windrose\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2014-10-20\cr
License: \tab MIT\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Thomas Hopper

Maintainer: Thomas Hopper <tomhopper@gmail.com>
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
data(wind_data)
wind_rose <- windrose(data = wind_data, spd = speed, dir = direction)
plot(wind_rose)
summary(wind_rose)
}
