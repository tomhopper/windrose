\name{windrose-package}
\alias{windrose-package}
\docType{package}
\title{
Provides manipulation and plotting of wind speed and direction data.
}
\description{
Accepts wind speed and direction data and produces an object that can then be plotted, printed or summarized using the functions \code{plot()}, \code{print()} and \code{summary()}. Based on code originally created by ___ and posted to Stack Overflow.
}
\details{
\tabular{ll}{
Package: \tab windrose\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2014-10-20\cr
License: \tab MIT\cr
}
\code{windrose()} produces the windrose object, which can then be plotted, printed or summarized.
\code{plot()} plots a windrose object.
}
\author{
Thomas Hopper

Maintainer: Thomas Hopper <tomhopper@gmail.com>
}
\references{
Original wind rose plot code created by Andy Clifton \url{http://stackoverflow.com/a/17266781}. 2013-06-24.
}
\keyword{ package }
}
\examples{
data(wind_data)
wind_rose <- windrose(data = wind_data, spd = speed, dir = direction)
plot(wind_rose)
summary(wind_rose)
}
