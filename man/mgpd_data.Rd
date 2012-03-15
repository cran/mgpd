\name{mgpd_data}
\alias{mgpd_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal
}
\description{
internal use only
}
\usage{
mgpd_data(xdat, thr = rep(0, 5))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xdat}{
%%     ~~Describe \code{xdat} here~~
}
  \item{thr}{
%%     ~~Describe \code{thr} here~~
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
function (xdat, thr = rep(0, 5)) 
{
    n = dim(xdat)[1]
    d = dim(xdat)[2]
    if (!(d \%in\% c(2:5))) 
        print("Error message: invalid dimension.")
    else {
        potdat = NULL
        id = rep(0, n)
        for (i in 1:d) potdat = cbind(potdat, xdat[, i] - thr[i])
        for (i in 1:d) id = id + (potdat[, i] > 0)
        potdat = potdat[id > 0, ]
    }
    potdat = as.data.frame(potdat)
    potdat
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
