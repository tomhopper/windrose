\name{summary.windrose}
\alias{summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prints a summary of a windrose object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A windrose object.
}
  \item{\dots}{
Other arguments are ignored.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(wind_data)
wind_rose <- windrose(data = wind_data, spd = Wind_Speed_meter_per_second, dir = Wind_Direction_deg)
summary(wind_rose)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
