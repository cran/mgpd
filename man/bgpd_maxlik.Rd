\name{bgpd_maxlik}
\alias{bgpd_maxlik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal
}
\description{
internal use only
}
\usage{
bgpd_maxlik(param, dat, model = "log", fixed = FALSE, psi = 3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param}{
%%     ~~Describe \code{param} here~~
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
function (param, dat, model = "log", fixed = FALSE, psi = 3, 
    ...) 
{
    mlmax = 1e+15
    models = c("log", "psilog", "philog", "neglog", "psineglog", 
        "phineglog", "bilog", "negbilog", "mix", "ct", "taj", 
        "smith")
    if (!(model \%in\% models)) 
        stop(paste("'", model, "' is not a valid model.", sep = ""))
    else {
        if (model == "log") 
            ml = ml_log(param = param, dat = dat, mlmax = mlmax, 
                fixed = fixed)
        if (model == "smith") 
            ml = ml_smith(param = param, dat = dat, mlmax = mlmax, 
                fixed = fixed)
        if (model == "psilog") 
            ml = ml_psilog(param = param, dat = dat, mlmax = mlmax, 
                fixed = fixed)
        if (model == "philog") 
            ml = ml_philog(param = param, dat = dat, mlmax = mlmax, 
                fixed = fixed)
        if (model == "neglog") 
            ml = ml_neglog(param = param, dat = dat, mlmax = mlmax, 
                fixed = fixed)
        if (model == "psineglog") 
            ml = ml_psineglog(param = param, dat = dat, mlmax = mlmax, 
                fixed = fixed)
        if (model == "phineglog") 
            ml = ml_phineglog(param = param, dat = dat, mlmax = mlmax, 
                fixed = fixed)
        if (model == "bilog") 
            ml = ml_bilog(param = param, dat = dat, mlmax = mlmax, 
                fixed = fixed)
        if (model == "negbilog") 
            ml = ml_negbilog(param = param, dat = dat, mlmax = mlmax, 
                fixed = fixed)
        if (model == "mix") 
            ml = ml_mix(param = param, dat = dat, mlmax = mlmax, 
                fixed = fixed)
        if (model == "ct") 
            ml = ml_ct(param = param, dat = dat, mlmax = mlmax, 
                fixed = fixed)
        if (model == "taj") 
            ml = ml_taj(param = param, dat = dat, mlmax = mlmax, 
                fixed = fixed)
        ml
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
