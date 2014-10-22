\name{windrose}
\alias{windrose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a windrose object
}
\description{
Create a windrose object from wind speed and direction data. Data can be passed as two vectors or as the column names of a data frame. Column names do not need to be quoted.
}
\usage{
windrose(data = NULL, spd, dir, spdres = NULL, dirres = 45, spdmin = NULL, spdmax = NULL, spdseq = NULL, palette = "YlGnBu", countmax = NA, plot_rose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
(optional) A data frame containing, at least, a column of wind speed data and a column of wind direction data.
}
  \item{spd}{
If \code{data} is provided, the name of the column containing wind speed data. If \code{data} is not provided, then a vector containing wind speed data.
}
  \item{dir}{
If \code{data} is provided, the name of the column containing wind direction data. If \code{data} is not provided, then a vector containing wind diretion data where \code{length(dir) == length(spd)}.
}
  \item{spdres}{
(optional) A numeric value indicating the number of bins to use with \code{spd} for plotting. If no value is given, defaults to 10 bins.
}
  \item{dirres}{
(optional) A numeric value indicating the number of degrees to use for each bin of \code{dir} for plotting. The default is 45 degrees, providing a plot with bins N, NE, E, SE, S, SW, W, NW.
}
  \item{spdmin}{
(optional) The minimum speed to use for calculating bin edges. All colors less than this value will be plotted in the same color. Ignored if spdseq is provided.
}
  \item{spdmax}{
(optional) The maximum speed to use for calculating bin edges. All colors greater than this value will be plotted in the same color. Ignored if spdseq is provided
}
  \item{spdseq}{
(optional) A numeric vector specifying the edges of bins to use during plotting wind speed.
}
  \item{palette}{
(optional) A Brewer Color palette, from package *RColorBrewer*, to use for plotting wind speed. Defaults to "YlGnBu".
}
  \item{countmax}{
(optional) Not used.
}
  \item{plot_rose}{
(optional) If TRUE, the rose plot will be printed when the windrose object is created. Defaults to FALSE, with \code{windrose()} only returning the windrose object.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of class windrose. May be treated as a list object.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Thomas Hopper
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot}}
\code{\link{summary}}
\code{\link{print}}
}
\examples{
## Using data frames
wind_df <- data.frame(speed = rlnorm(1000, 0, 0.75), direction = runif(1000, 0, 360))
wind_rose <- windrose(data = wind_df, spd = speed, dir = direction)

## Using vectors
speed_v <- rlnorm(1000, 0, 0.75)
direction_v <- runif(1000, 0, 360)
wind_rose <- windrose(spd = speed_v, dir = direction_v)
  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
