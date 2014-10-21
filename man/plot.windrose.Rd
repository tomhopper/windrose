\name{plot}
\alias{plot}
\title{
Plots a windrose object on a custom rose plot
}
\description{
Plots a rose plot for wind data created with \code{windrose()}, with fill colors based on corresponding wind speed.
}
\usage{
plot(wind_rose)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Not used. Included for compatibility with base \code{plot()}.
}
  \item{y}{
Not used. Included for compatibility with base \code{plot()}.
}
  \item{data}{
%%     ~~Describe \code{data} here~~
A windrose object.
}
  \item{palette}{
%%     ~~Describe \code{palette} here~~
The Brewer color palette to use. If NULL, uses the palette specified by \code{windrose()}.
}
  \item{plot_theme}{
%%     ~~Describe \code{plot_theme} here~~
The ggplot2 theme to use for plotting. Supports ggplot2 built-in themes, ggthemes and xkcd.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
Other arguments are not used.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a ggplot object
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Thomas Hopper
}
\note{
%%  ~~further notes~~
Based on code provided by Andy Clifton on Stack Overflow \url{http://stackoverflow.com/a/17266781}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{windrose}}
\code{\link{RColorBrewer}}
\code{\link{ggplot}}
}
\examples{
##Create a windrose object and plot it.
wind_df <- data.frame(speed = rlnorm(1000, 0, 0.75), direction = runif(1000, 0, 360))
wind_rose <- windrose(data = wind_df, spd = speed, dir = direction)
plot(wind_rose)

##With the built-in data set:
data(wind_data)
wind_rose <- windrose(data = wind_data, spd = Wind_Speed_meter_per_second, dir = Wind_Direction_deg)
plot(wind_rose)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
