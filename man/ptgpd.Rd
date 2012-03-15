\name{ptgpd}
\alias{ptgpd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal
}
\description{
internal use only
}
\usage{
ptgpd(x, y, z, model = "log", mar1 = c(0, 1, 0.1), mar2 = c(0, 1, 0.1), mar3 = c(0, 1, 0.1), dep = 1.5, A1 = 0, A2 = 0, B1 = 3, B2 = 3, checkconv = TRUE, ...)
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
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{mar1}{
%%     ~~Describe \code{mar1} here~~
}
  \item{mar2}{
%%     ~~Describe \code{mar2} here~~
}
  \item{mar3}{
%%     ~~Describe \code{mar3} here~~
}
  \item{dep}{
%%     ~~Describe \code{dep} here~~
}
  \item{A1}{
%%     ~~Describe \code{A1} here~~
}
  \item{A2}{
%%     ~~Describe \code{A2} here~~
}
  \item{B1}{
%%     ~~Describe \code{B1} here~~
}
  \item{B2}{
%%     ~~Describe \code{B2} here~~
}
  \item{checkconv}{
%%     ~~Describe \code{checkconv} here~~
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
function (x, y, z, model = "log", mar1 = c(0, 1, 0.1), mar2 = c(0, 
    1, 0.1), mar3 = c(0, 1, 0.1), dep = 1.5, A1 = 0, A2 = 0, 
    B1 = 3, B2 = 3, checkconv = TRUE, ...) 
{
    models = c("log", "psilog", "neglog", "psineglog")
    if (!(model \%in\% models)) 
        stop(paste("'", model, "' is not a valid model.", sep = ""))
    else {
        if (model == "log") 
            ptgpd = ptgpd_log(x, y, z, mar1 = mar1, mar2 = mar2, 
                mar3 = mar3, dep = dep)
        if (model == "psilog") 
            ptgpd = ptgpd_psilog(x, y, z, mar1 = mar1, mar2 = mar2, 
                mar3 = mar3, dep = dep, A1 = A1, A2 = A2, B1 = B1, 
                B2 = B2, checkconv = checkconv)
        if (model == "neglog") 
            ptgpd = ptgpd_neglog(x, y, z, mar1 = mar1, mar2 = mar2, 
                mar3 = mar3, dep = dep)
        if (model == "psineglog") 
            ptgpd = ptgpd_psineglog(x, y, z, mar1 = mar1, mar2 = mar2, 
                mar3 = mar3, dep = dep, A1 = A1, A2 = A2, B1 = B1, 
                B2 = B2, checkconv = checkconv)
        ptgpd
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
