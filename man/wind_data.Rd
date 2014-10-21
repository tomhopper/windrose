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
    \item{\code{DateTime}}{POSIXct date and time data for each measurement of wind speed and direction.}
    \item{\code{Wind_Speed_meter_per_second}}{Numeric wind speed data, in meters per second.}
    \item{\code{Wind_Direction_deg}}{Numeric wind direction data, in degrees (0 - 360).}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Downloaded from MAREC real-time data monitoring \url{http://datamonitoring.marec.gvsu.edu}.
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
