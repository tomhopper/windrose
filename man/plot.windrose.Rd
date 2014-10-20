\name{plot.windrose}
\alias{plot.windrose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.windrose(x = NULL, y = NULL, data, palette = NULL, plot_theme = "theme_minimal", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{palette}{
%%     ~~Describe \code{palette} here~~
}
  \item{plot_theme}{
%%     ~~Describe \code{plot_theme} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x = NULL, y = NULL, data, palette = NULL, plot_theme = "theme_minimal", 
    ...) 
{
    if (plot_theme == "theme_grey") 
        plot_theme <- "theme_gray"
    if (is.null(plot_theme)) 
        plot_theme <- "theme_minimal"
    if (!is.null(palette)) {
        n_spd_seq <- length(data$spd_colors)
        if ("gray50" \%in\% data$spd_colors) {
            n_spd_seq <- n_spd_seq - 1
            add_gray <- TRUE
        }
        else add_gray <- FALSE
        n_colors_in_range <- n_spd_seq
        spd_colors <- colorRampPalette(brewer.pal(min(max(3, 
            n_colors_in_range), min(9, n_colors_in_range)), palette))(n_colors_in_range)
        if (add_gray) 
            spd_colors <- c(spd_colors, "gray50")
        data$spd_colors <- spd_colors
        rm(add_gray, n_spd_seq, n_colors_in_range, spd_colors)
    }
    p_windrose <- ggplot(data = na.omit(data$data), aes(x = dir_binned, 
        fill = spd_binned)) + geom_bar() + scale_x_discrete(drop = FALSE, 
        labels = waiver()) + coord_polar(start = -((data$dirres/2)/360) * 
        2 * pi) + scale_fill_manual(name = expression(paste("Wind Speed / m s"^~-1)), 
        values = data$spd_colors, drop = FALSE)
    if (!is.na(data$countmax)) {
        p_windrose <- p_windrose + ylim(c(0, data$countmax))
    }
    switch(EXPR = plot_theme, theme_gray = p_windrose <- p_windrose + 
        theme_gray(), theme_bw = p_windrose <- p_windrose + theme_bw(), 
        theme_linedraw = p_windrose <- p_windrose + theme_linedraw(), 
        theme_light = p_windrose <- p_windrose + theme_light(), 
        theme_minimal = p_windrose <- p_windrose + theme_minimal(), 
        theme_classic = p_windrose <- p_windrose + theme_classic(), 
        theme_xkcd = {
            if (xkcd_loaded) p_windrose <- p_windrose + ggthemes::theme_xkcd() else p_windrose <- p_windrose + 
                theme_gray()
        }, theme_economist = {
            if (ggthemes_loaded) p_windrose <- p_windrose + ggthemes::theme_economist() else p_windrose <- p_windrose + 
                theme_gray()
        }, theme_excel = {
            if (ggthemes_loaded) p_windrose <- p_windrose + ggthemes::theme_excel() else p_windrose <- p_windrose + 
                theme_gray()
        }, theme_few = {
            if (ggthemes_loaded) p_windrose <- p_windrose + ggthemes::theme_few() else p_windrose <- p_windrose + 
                theme_gray()
        }, theme_solarized = {
            if (ggthemes_loaded) p_windrose <- p_windrose + ggthemes::theme_solarized() else p_windrose <- p_windrose + 
                theme_gray()
        }, theme_tufte = {
            if (ggthemes_loaded) p_windrose <- p_windrose + ggthemes::theme_tufte() else p_windrose <- p_windrose + 
                theme_gray()
        }, theme_wsj = {
            if (ggthemes_loaded) p_windrose <- p_windrose + ggthemes::theme_wsj() else p_windrose <- p_windrose + 
                theme_gray()
        }, theme_gray())
    p_windrose <- p_windrose + theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
    return(p_windrose)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line