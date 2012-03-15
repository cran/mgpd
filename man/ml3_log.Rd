\name{ml3_log}
\alias{ml3_log}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal
}
\description{
internal use only
}
\usage{
ml3_log(param, dat, mlmax = 1e+15, fixed = FALSE, ...)
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
    lik = NULL
    x = dat[, 1]
    y = dat[, 2]
    z = dat[, 3]
    if (fixed) 
        param[1] = 0
    lik = try(dtgpd_log(x, y, z, mar1 = param[1:3], mar2 = param[4:6], 
        mar3 = param[7:9], dep = param[10]))
    if (!is.null(lik)) {
        loglik = -sum(log(lik))
        if (min(1 + param[3] * (x - param[1])/param[2]) < 0) 
            loglik = mlmax
        if (min(1 + param[6] * (y - param[4])/param[5]) < 0) 
            loglik = mlmax
        if (min(1 + param[9] * (z - param[7])/param[8]) < 0) 
            loglik = mlmax
    }
    loglik
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
