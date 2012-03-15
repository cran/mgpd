\name{dbgpd_region}
\alias{dbgpd_region}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal
}
\description{
internal use only
}
\usage{
dbgpd_region(x, y, z, quant = c(0.95, 0.9, 0.75), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{z}{
%%     ~~Describe \code{z} here~~
}
  \item{quant}{
%%     ~~Describe \code{quant} here~~
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
function (x, y, z, quant = c(0.95, 0.9, 0.75), ...) 
{
    prec = 1000
    sq = (x[2] - x[1]) * (y[2] - y[1])
    one = sum(z) * sq
    level = seq(quantile(z, 0.4, na.rm = TRUE), quantile(z, 0.9995, 
        na.rm = TRUE), length.out = prec)
    numint = rep(0, prec)
    levelf = function(l) sum(z[z > l]) * sq
    numint = sapply(level, levelf)/one
    nq = rep(0, length(quant))
    for (i in 1:length(quant)) nq[i] = level[numint <= quant[i]][1]
    out = list(one, quant, nq, x, y, z)
    names(out) = c("one", "prob", "q", "x", "y", "z")
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
