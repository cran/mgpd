\name{pbgpd}
\alias{pbgpd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal
}
\description{
internal use only
}
\usage{
pbgpd(x, y, model = "log", mar1 = c(0, 1, 0.1), mar2 = c(0, 1, 0.1), dep = 2, a = 1/2, b = 1/2, asy = 0, p = 3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{mar1}{
%%     ~~Describe \code{mar1} here~~
}
  \item{mar2}{
%%     ~~Describe \code{mar2} here~~
}
  \item{dep}{
%%     ~~Describe \code{dep} here~~
}
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
  \item{asy}{
%%     ~~Describe \code{asy} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
internal use only
}
\value{
internal use only
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
internal use only
}
\author{
P. Rakonczai
}
\note{
internal use only
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
internal use only
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y, model = "log", mar1 = c(0, 1, 0.1), mar2 = c(0, 
    1, 0.1), dep = 2, a = 1/2, b = 1/2, asy = 0, p = 3, ...) 
{
    pbgpd = NULL
    models = c("log", "psilog", "philog", "neglog", "psineglog", 
        "phineglog", "bilog", "negbilog", "ct", "taj")
    if (!(model \%in\% models)) 
        stop(paste("'", model, "' is an unknown model.", sep = ""))
    else {
        if (model == "log") 
            pbgpd = pbgpd_log(x, y, mar1 = mar1, mar2 = mar2, 
                dep = dep)
        if (model == "psilog") 
            pbgpd = pbgpd_psilog(x, y, mar1 = mar1, mar2 = mar2, 
                dep = dep, asy = asy, p = p)
        if (model == "philog") 
            pbgpd = pbgpd_philog(x, y, mar1 = mar1, mar2 = mar2, 
                dep = dep, asy = asy, p = p)
        if (model == "neglog") 
            pbgpd = pbgpd_neglog(x, y, mar1 = mar1, mar2 = mar2, 
                dep = dep)
        if (model == "psineglog") 
            pbgpd = pbgpd_psineglog(x, y, mar1 = mar1, mar2 = mar2, 
                dep = dep, asy = asy, p = p)
        if (model == "phineglog") 
            pbgpd = pbgpd_phineglog(x, y, mar1 = mar1, mar2 = mar2, 
                dep = dep, asy = asy, p = p)
        if (model == "bilog") 
            pbgpd = pbgpd_bilog(x, y, mar1 = mar1, mar2 = mar2, 
                dep = dep, a = a, b = b)
        if (model == "negbilog") 
            pbgpd = pbgpd_negbilog(x, y, mar1 = mar1, mar2 = mar2, 
                dep = dep, a = a, b = b)
        if (model == "ct") 
            pbgpd = pbgpd_ct(x, y, mar1 = mar1, mar2 = mar2, 
                a = a, b = b)
        if (model == "taj") 
            pbgpd = pbgpd_taj(x, y, mar1 = mar1, mar2 = mar2, 
                a = a, b = b)
    }
    pbgpd
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
