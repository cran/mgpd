\name{mgpd_data}
\alias{mgpd_data}
\title{
Exceedances over given threshold vector for MGPD methods
}
\description{
Selecting those observations from a data matrix which are over the threshold at least in one of the components. These exceedances are to be modelled 
by multivariate Pareto distribution of Type II.
}
\usage{
mgpd_data(xdat, thr = rep(0, 5))
}
\arguments{
  \item{xdat}{
Matrix of observations.
}
  \item{thr}{
Vector of thresholds.
}
}
\author{
Pal Rakonczai <rakonczai.p@gmail.com>
}
\examples{
data(DailyWind)
demodata       = DailyWind[ , c(4, 6) ]
nms            = names( demodata )
thr            = apply( demodata, 2, quantile, prob=0.95 )
potdata        = mgpd_data( demodata, thr=thr )

plot( potdata[,1]+thr[1], potdata[,2]+thr[2], xlab=nms[1], ylab=nms[2], main="Exceedances" )
abline( h=thr[2], v=thr[1] )
}
