\name{pbgpd_ct}
\alias{pbgpd_ct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal
}
\description{
internal use only
}
\usage{
pbgpd_ct(x, y, mar1 = c(0, 1, 0.1), mar2 = c(0, 1, 0.1), a = 1/2, b = 1/2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{mar1}{
%%     ~~Describe \code{mar1} here~~
}
  \item{mar2}{
%%     ~~Describe \code{mar2} here~~
}
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
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
function (x, y, mar1 = c(0, 1, 0.1), mar2 = c(0, 1, 0.1), a = 1/2, 
    b = 1/2, ...) 
{
    param = as.numeric(c(mar1, mar2, a, b))
    mux = param[1]
    muy = param[4]
    sigx = param[2]
    sigy = param[5]
    gamx = param[3]
    gamy = param[6]
    a = param[7]
    b = param[8]
    Hxy = NULL
    error = FALSE
    if (sigx < 0 | sigy < 0 | a < 0 | b < 0) 
        error = TRUE
    if (!error) {
        Hxy = NA
        c0 = log(pbvevd(c(0, 0), model = "ct", mar1 = c(mux, 
            sigx, gamx), mar2 = c(muy, sigy, gamy), alpha = a, 
            beta = b))
        Hxy = -1/c0 * log(pbvevd(c(x, y), model = "ct", mar1 = c(mux, 
            sigx, gamx), mar2 = c(muy, sigy, gamy), alpha = a, 
            beta = b)/pbvevd(cbind(pmin(x, 0), pmin(y, 0)), model = "ct", 
            mar1 = c(mux, sigx, gamx), mar2 = c(muy, sigy, gamy), 
            alpha = a, beta = b))
    }
    else stop("invalid parameter(s)")
    Hxy
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
