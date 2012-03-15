\name{mgpd-package}
\alias{mgpd-package}
\alias{mgpd}
\docType{package}
\title{
mgpd: Functions for multivariate generalized Pareto distribution (MGPD of Type II)
}
\description{
Extends distribution and density functions to parametric multivariate generalized Pareto distributions 
(MGPD of Type II), and provides fitting functions which calculate maximum likelihood 
estimates for bivariate and trivariate models. 
}
\details{
\tabular{ll}{
Package: \tab mgpd\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2012-03-07\cr
License: \tab 	GPL-3 \cr
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
Pal Rakonczai
Maintainer: Pal Rakonczai <rakonczai.p [at] gmail.com>
}
\references{
Rakonczai and Zempleni (2010)
}
\keyword{ dbgpd, dtgpd}

\examples{
x=y=seq(-2,6,0.05)
z1=outer(x,y,pbgpd,model="log")
z2=outer(x,y,pbgpd,model="neglog")
image(z1-z2)
}
