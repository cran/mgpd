\name{dbgpd}
\alias{dbgpd}
\title{
Density function for bivariate generalized Pareto distribution (of Type II) 
}
\description{
Density function for BGPD of Type II. See Details below for further explanation on Type II.   
}
\usage{
dbgpd(x, y, model = "log", mar1 = c(0, 1, 0.1), mar2 = c(0, 1, 0.1), dep = 2, a = 1, b = 1, asy = 0, ...)
}
\arguments{
  \item{x}{
Vector of values for the first component.
}
  \item{y}{
Vector of values for the second component.
}
  \item{model}{
Parametric model for the underlying dependence structure. The \code{log}, \code{neglog}, \code{taj}, \code{mix} are symmetric 
and \code{psilog}, \code{psineglog}, \code{bilog}, \code{negbilog}, \code{ct} are asymmetric models.
}
  \item{mar1,mar2}{
Vectors of length three containing marginal parameters (location, scale and shape) of the underlying EVD model.
}
  \item{dep}{
Value for the dependence parameter in the \code{log}, \code{neglog}, \code{psilog} and \code{psineglog} models.
}
  \item{a,b}{
Further parameter values of \code{taj}, \code{bilog}, \code{negbilog} and \code{ct} models.
}
  \item{asy}{
Asymmetry parameter in \code{psilog} and \code{psilog} models.
}
 \item{...}{
Further arguments to be passed.
}
}
\details{
There are several (non-equivalent) definitions of MGPD in use, one is for exceedances over a threshold
in at least one of the components (Type II) and another(s) for exceedances which are over
the threshold in all components (Type I). Type II is implemented in \code{mgpd} according 
to Rootzen and Tajvidi (2006), see \code{fbvpot} in \code{evd} package for Type I.
}
\value{
Returns the value of the density.  
}
\author{
Pal Rakonczai <rakonczai.p@gmail.com>
}

\seealso{\code{\link{dbgpd_region}}, \code{\link{bgpd_fit}}}

\examples{

#1. Drawing density contours

x=y=seq(-4,4,0.05)
z=outer(x,y,dbgpd,model="log",dep=2)
contour(x,y,z,drawlabel=FALSE,col="gray",main="BGPD density")
abline(h=0,v=0,lty=2)
# the following model is still the same, if asy=0
z=outer(x,y,dbgpd,model="psilog",dep=2,asy=0)
contour(x,y,z,add=TRUE,col=1,drawlabel=FALSE)
# asymmetric models
z=outer(x,y,dbgpd,model="psilog",dep=2,asy=0.3)
contour(x,y,z,add=TRUE,col=4,drawlabel=FALSE)
z=outer(x,y,dbgpd,model="psilog",dep=2,asy=-0.3)
contour(x,y,z,add=TRUE,col=3,drawlabel=FALSE)
legend("bottomleft",c(expression(psi==0.3),expression(psi==0),expression(psi==-0.3)),lty=1,col=c(4,1,3),title="Psi-logistic")  

#2. Density images for couple of models 

par(mfrow=c(2,2))
z=outer(x,  y,  dbgpd,  model="bilog",    a=1/2,b=1/2)    # a bit slow
image(x,y,z,main="Bilog")
z=outer(x,  y,  dbgpd,  model="negbilog", a=1/2,b=1/2)    # a bit slow
image(x,y,z,main="Negbilog")
z=outer(x,  y,  dbgpd,  model="ct",       a=1/2,b=1/2)
image(x,y,z,main="Coles-Tawn")
z=outer(x,  y,  dbgpd_taj, a=2, b=0.1)
image(x,y,z,main="Tajvidi's")
}

