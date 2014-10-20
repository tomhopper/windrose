\name{wind_data}
\alias{wind_data}
\docType{data}
\title{
Sample wind data
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("wind_data")}
\format{
  A data frame with 90815 observations on the following 3 variables.
  \describe{
    \item{\code{DateTime}}{a POSIXct}
    \item{\code{Wind_Speed_meter_per_second}}{a numeric vector}
    \item{\code{Wind_Direction_deg}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(wind_data)
wind_rose <- windrose(data = wind_data, spd = Wind_Speed_meter_per_second, dir = Wind_Direction_deg)
plot(wind_rose)
}
\keyword{datasets}
