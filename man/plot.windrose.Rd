\name{plot}
\alias{plot.windrose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots a windrose object on a custom rose plot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot(wind_rose)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Not used.
}
  \item{y}{
Not used.
}
  \item{data}{
%%     ~~Describe \code{data} here~~
A windrose object.
}
  \item{palette}{
%%     ~~Describe \code{palette} here~~
The Brewer color palette to use. If NULL, uses the palette specified by \code{windrose()}
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
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
wind_df <- data.frame(speed = rlnorm(1000, 0, 0.75), direction = runif(1000, 0, 360))
wind_rose <- windrose(data = wind_df, spd = speed, dir = direction)
plot(wind_rose)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
