\name{dbgpd_region}
\alias{dbgpd_region}
\title{
Computing BGPD predction regions
}
\description{
Computing prediction region (Hall and Tajvidi, 2000) for a given probability.  This region is bounded by a horizontal level curve
of the BGPD density over which the integral of the density equals to the given probability. 
}
\usage{
dbgpd_region(x,y,z,quant=c(.95,0.9,.75), ...)
}
\arguments{
  \item{x,y}{
Vectors of grid coordinates, on which the BGPD density is evaluated.   
}
\item{z}{
Matrix containing BGPD density values evaluated on a grid. Typically obtainable from \code{dbgpd}, see examples below.   
}
  \item{quant}{
Vector of probabilities.
}
 \item{...}{
Further arguments to be passed.
}
}


\references{
Rakonczai, P. (2011) Bivariate generalized Pareto distribution in practice: models and estimation. (submitted)

Rakonczai, P. and Tajvidi, N. (2010) On prediction of bivariate extremes. \emph{International Journal of Intelligent Technologies and Applied Statistics} 3(2): 115-139.

Hall, P. and Tajvidi, N. (2000) Prediction regions for bivariate extreme events. \emph{Aust. N. Z. J. Stat.} 46(1): 99-112.
}
\author{
Pal Rakonczai <rakonczai.p@gmail.com>
}

\seealso{\code{\link{dbgpd}}, \code{\link{bgpd_fit}}}

\examples{
# prediction regions from given bgpd density
x       = y = seq(  -4, 4,  0.02  )
z       = outer(  x,  y,  dbgpd,  model="log",  dep=2 )
log.reg = dbgpd_region( x,  y,  z )

par(  mfrow=c(  1,  1 ) )
contour(  log.reg$x,  log.reg$y,  log.reg$z,  levels=log.reg$q, drawlabels=FALSE, main="Prediction Regions",  col=c(1,3,4)  )
abline( h=0,  v=0,  lty=2 )
legend( "bottomleft", c(expression(gamma==0.95),  expression(gamma==0.9), expression(gamma==0.75)), lty=1,  col=c(1,3,4), title="Regions")  
}

