\name{ml_negbilog}
\alias{ml_negbilog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal
}
\description{
internal use only
}
\usage{
ml_negbilog(param, dat, mlmax = 1e+15, fixed = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param}{
%%     ~~Describe \code{param} here~~
}
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{mlmax}{
%%     ~~Describe \code{mlmax} here~~
}
  \item{fixed}{
%%     ~~Describe \code{fixed} here~~
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
function (param, dat, mlmax = 1e+15, fixed = FALSE, ...) 
{
    loglik = mlmax
    hxy = NA
    x = dat[, 1]
    y = dat[, 2]
    error = FALSE
    mux = param[1]
    muy = param[4]
    sigx = param[2]
    sigy = param[5]
    gamx = param[3]
    gamy = param[6]
    a = param[7]
    b = param[8]
    if (sigx < 0 | sigy < 0 | a > 0 | b > 0) 
        error = TRUE
    if (fixed == TRUE) {
        mux = 0
    }
    if (error) 
        loglik = mlmax
    if (!error) {
        tx = (1 + gamx * (x - mux)/sigx)^(1/gamx)
        ty = (1 + gamy * (y - muy)/sigy)^(1/gamy)
        tx0 = (1 + gamx * (-mux)/sigx)^(1/gamx)
        ty0 = (1 + gamy * (-muy)/sigy)^(1/gamy)
        dtx = (1/sigx) * pmax((1 + gamx * (x - mux)/sigx), 0)^(1/gamx - 
            1)
        dty = (1/sigy) * pmax((1 + gamy * (y - muy)/sigy), 0)^(1/gamy - 
            1)
        w = tx/(tx + ty)
        l = length(w)
        gma1 = rep(NA, l)
        if (any(is.na(w))) 
            loglik = mlmax
        else for (i in 1:l) {
            eqn = function(z) (1 - a) * (1 - w[i]) * (1 - z)^b - 
                (1 - b) * w[i] * z^a
            if (w[i] == 0) 
                gma1[i] = 0
            else if (w[i] == 1) 
                gma1[i] = 1
            else gma1[i] = uniroot(eqn, lower = 0, upper = 1, 
                tol = .Machine$double.eps^0.5)$root
        }
        hdens = function(w, gma = gma1) -((1 - a) * (1 - gma) * 
            gma^(1 - a))/((1 - w) * w^2 * ((1 - gma) * a + gma * 
            b))
        dxdymu = function(x1, y1) -(x1 + y1)^(-3) * hdens(x1/(x1 + 
            y1))
        c0 = log(pbvevd(c(0, 0), model = "negbilog", mar1 = c(mux, 
            sigx, gamx), mar2 = c(muy, sigy, gamy), alpha = (-a), 
            beta = (-b)))
        hxy = 1/c0 * dxdymu(tx, ty) * dtx * dty
        hxy = as.numeric(hxy * (1 - ((x < 0) * (y < 0))))
        loglik = -sum(log(hxy))
    }
    if (min(1 + gamx * (x - mux)/sigx) < 0) 
        loglik = mlmax
    if (min(1 + gamy * (y - muy)/sigy) < 0) 
        loglik = mlmax
    loglik
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
