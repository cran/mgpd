\name{fbgpd}
\alias{fbgpd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal
}
\description{
internal use only
}
\usage{
fbgpd(initpar, dat, model = "log", fixed = FALSE, control = list(maxit = 50000), psi = 3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initpar}{
%%     ~~Describe \code{initpar} here~~
}
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{fixed}{
%%     ~~Describe \code{fixed} here~~
}
  \item{control}{
%%     ~~Describe \code{control} here~~
}
  \item{psi}{
%%     ~~Describe \code{psi} here~~
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
function (initpar, dat, model = "log", fixed = FALSE, control = list(maxit = 50000), 
    psi = 3, ...) 
{
    est = optim(initpar, bgpd_maxlik, dat = dat, model = model, 
        control = control, fixed = fixed, psi = psi)
    if (fixed == TRUE) 
        est$par[1] = 0
    est
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
