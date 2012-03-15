\name{dbgpd_ct}
\alias{dbgpd_ct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal
}
\description{
internal use only
}
\usage{
dbgpd_ct(x, y, mar1 = c(0, 1, 0.1), mar2 = c(0, 1, 0.1), a = 1/2, b = 1/2, ...)
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
    Be1 = function(x) pbeta(x, a + 1, b)
    Be2 = function(x) pbeta(x, a, b + 1)
    d1Be1 = function(x) dbeta(x, a + 1, b)
    d1Be2 = function(x) dbeta(x, a, b + 1)
    d2 = function(x, a1, b1) x^(a1 - 1) * (a1 - 1) * (1 - x)^(b1 - 
        1)/(x * beta(a1, b1)) - x^(a1 - 1) * (1 - x)^(b1 - 1) * 
        (b1 - 1)/((1 - x) * beta(a1, b1))
    d2Be1 = function(x) d2(x, a + 1, b)
    d2Be2 = function(x) d2(x, a, b + 1)
    Qxy = function(x, y) a * 1/y/(a * 1/y + b * 1/x)
    dxQxy = function(x, y) a * b/(y * (a/y + b/x)^2 * x^2)
    dyQxy = function(x, y) -a/(y^2 * (a/y + b/x)) + a^2/(y^3 * 
        (a/y + b/x)^2)
    dxdyQxy = function(x, y) -a * b/(y^2 * (a/y + b/x)^2 * x^2) + 
        2 * a^2 * b/(y^3 * (a/y + b/x)^3 * x^2)
    mu = function(x, y) 1/x * (1 - Be1(Qxy(x, y))) + 1/y * Be2(Qxy(x, 
        y))
    dxdymu = function(x, y) 1/x^2 * d1Be1(Qxy(x, y)) * dyQxy(x, 
        y) - 1/x * d2Be1(Qxy(x, y)) * dxQxy(x, y) * dyQxy(x, 
        y) - 1/x * d1Be1(Qxy(x, y)) * dxdyQxy(x, y) - 1/y^2 * 
        d1Be2(Qxy(x, y)) * dxQxy(x, y) + 1/y * d2Be2(Qxy(x, y)) * 
        dxQxy(x, y) * dyQxy(x, y) + 1/y * d1Be2(Qxy(x, y)) * 
        dxdyQxy(x, y)
    param = as.numeric(c(mar1, mar2, a, b))
    mux = param[1]
    muy = param[4]
    sigx = param[2]
    sigy = param[5]
    gamx = param[3]
    gamy = param[6]
    a = param[7]
    b = param[8]
    hxy = NULL
    error = FALSE
    if (sigx < 0 | sigy < 0 | a < 0 | b < 0) 
        error = TRUE
    if (!error) {
        hxy = NA
        tx = (1 + gamx * (x - mux)/sigx)^(1/gamx)
        ty = (1 + gamy * (y - muy)/sigy)^(1/gamy)
        tx0 = (1 + gamx * (-mux)/sigx)^(1/gamx)
        ty0 = (1 + gamy * (-muy)/sigy)^(1/gamy)
        dtx = (1/sigx) * pmax((1 + gamx * (x - mux)/sigx), 0)^(1/gamx - 
            1)
        dty = (1/sigy) * pmax((1 + gamy * (y - muy)/sigy), 0)^(1/gamy - 
            1)
        c0 = -mu(tx0, ty0)
        hxy = 1/c0 * dxdymu(tx, ty) * dtx * dty
        hxy = as.numeric(hxy * (1 - ((x < 0) * (y < 0))))
    }
    else stop("invalid parameter(s)")
    hxy
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
