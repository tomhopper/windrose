\name{windrose}
\alias{windrose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
windrose(data = NULL, spd, dir, spdres = NULL, dirres = 45, spdmin = NULL, spdmax = NULL, spdseq = NULL, palette = "YlGnBu", countmax = NA, plot_rose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{spd}{
%%     ~~Describe \code{spd} here~~
}
  \item{dir}{
%%     ~~Describe \code{dir} here~~
}
  \item{spdres}{
%%     ~~Describe \code{spdres} here~~
}
  \item{dirres}{
%%     ~~Describe \code{dirres} here~~
}
  \item{spdmin}{
%%     ~~Describe \code{spdmin} here~~
}
  \item{spdmax}{
%%     ~~Describe \code{spdmax} here~~
}
  \item{spdseq}{
%%     ~~Describe \code{spdseq} here~~
}
  \item{palette}{
%%     ~~Describe \code{palette} here~~
}
  \item{countmax}{
%%     ~~Describe \code{countmax} here~~
}
  \item{plot_rose}{
%%     ~~Describe \code{plot_rose} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data = NULL, spd, dir, spdres = NULL, dirres = 45, 
    spdmin = NULL, spdmax = NULL, spdseq = NULL, palette = "YlGnBu", 
    countmax = NA, plot_rose = FALSE) 
{
    if (is.null(data)) {
        if (is.numeric(spd) & is.numeric(dir)) {
            data <- data.frame(spd = spd, dir = dir)
            spd <- "spd"
            dir <- "dir"
        }
    }
    else {
        if (class(data) == "data.frame") {
            spd <- deparse(substitute(spd))
            dir <- deparse(substitute(dir))
        }
        else {
            stop("Please pass a data frame to parameter 'data'.")
        }
    }
    if (is.null(spdmin)) 
        spdmin <- min(data[[spd]], na.rm = TRUE)
    if (is.null(spdmax)) 
        spdmax <- max(data[[spd]], na.rm = TRUE)
    n_in <- NROW(data)
    dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
    data[[spd]][dnu] <- NA
    data[[dir]][dnu] <- NA
    if (is.null(spdseq)) {
        if (is.null(spdres)) 
            spdres <- (spdmax - spdmin)/10
        spdseq <- round(seq(spdmin, spdmax, spdres), digits = 1)
    }
    else {
    }
    n_spd_seq <- length(spdseq)
    n_colors_in_range <- n_spd_seq - 1
    spd_colors <- colorRampPalette(brewer.pal(min(max(3, n_colors_in_range), 
        min(9, n_colors_in_range)), palette))(n_colors_in_range)
    if (max(data[[spd]], na.rm = TRUE) > spdmax) {
        spd_breaks <- c(spdseq, max(data[[spd]], na.rm = TRUE))
        spd_labels <- c(paste(c(spdseq[1:n_spd_seq - 1]), "-", 
            c(spdseq[2:n_spd_seq])), paste(spdmax, "-", max(data[spd], 
            na.rm = TRUE)))
        spd_colors <- c(spd_colors, "grey50")
    }
    else {
        spd_breaks <- c(seq(spdseq))
        spd_labels <- paste(c(spdseq[1:n_spd_seq - 1]), "-", 
            c(spdseq[2:n_spd_seq]))
    }
    data$spd_binned <- cut(x = data[[spd]], breaks = spd_breaks, 
        labels = spd_labels, ordered_result = TRUE)
    dir_breaks <- c(-dirres/2, seq(dirres/2, 360 - dirres/2, 
        by = dirres), 360 + dirres/2)
    dir_labels <- c(paste(round(360 - dirres/2, 0), "-", round(dirres/2, 
        0)), paste(round(seq(dirres/2, 360 - 3 * dirres/2, by = dirres), 
        0), "-", round(seq(3 * dirres/2, 360 - dirres/2, by = dirres), 
        0)), paste(round(360 - dirres/2, 0), "-", round(dirres/2, 
        0)))
    dir_binned <- cut(data[[dir]], breaks = dir_breaks, ordered_result = TRUE)
    levels(dir_binned) <- dir_labels
    data$dir_binned <- dir_binned
    wind_data <- structure(list(data = data, dirres = dirres, 
        countmax = countmax, spd_colors = spd_colors), class = "windrose")
    attr(wind_data, "hidden") <- c("dirres", "countmax", "spd_colors")
    if (plot_rose == TRUE) 
        plot(data)
    return(wind_data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
