\name{bgpd_fit}
\alias{bgpd_fit}
\title{
Maximum-likelihood fitting for the bivariate generalized Pareto distribution
}
\description{
Provides maximum likelihood estimates for BGPD models. There are seven "classical" parametric dependence models within BGPD available: 
logistic (Tawn, 1988), negative logistic (Joe, 1990), bilogistic (Smith, 1990), 
negative bilogistic (Coles and Tawn, 1994), Tajvidi's generalised symmetric logistic (Tajvidi, 1996), Dirichlet (Coles and Tawn, 1991),
mixed (Kluppelberg and May, 1999). Beyond these, two novel asymmetric approaches giving absolutely continuous MGPD are
implemented as \emph{psi-logistic} and \emph{psi-negative logistic} (Rakonczai, 2011). 
}
\usage{
bgpd_fit(initpar, dat, model="log", fixedmu=FALSE, control=list(maxit=50000),...)
}
\arguments{
\item{initpar}{
Initial values for the parameters over which the likelihood optimalization is performed.
}
  \item{dat}{
Matrix of observations (with two coloumns).
}
  \item{model}{
Parametric model for the underlying dependence structure. The \code{log}, \code{neglog}, \code{taj}, \code{mix} are symmetric 
and \code{psilog}, \code{psineglog}, \code{bilog}, \code{negbilog}, \code{ct} are asymmetric models.
}
  \item{fixedmu}{
If TRUE, then the location parameter of the x margin is kept to be 0 through the opimization. 
}
 \item{control}{
A list of control parameters. E.g. maxit is the maximum number of iterations.
}
 \item{...}{
Further arguments to be passed.
}
}

\value{
Returns a list containing the maximum likelihood estimates, log-likelihood value and further information about the optimalization process. 
See \code{optim} for details.
}

\references{
Coles, S.G. and Tawn, J.A. (1991) Modelling extreme multivariate events. \emph{Journal of the Royal
Statistical Society}, Series B 53.

Coles, S.G. and Tawn, J.A. (1994) Statistical methods for multivariate extremes: an application
to structural design. \emph{Applied Statistics} 43.

Joe, H. (1990) Families of min-stable multivariate exponential and multivariate extreme value
distributions. \emph{Statistics & Probability Letters} 9.

Kluppelberg, C. and May, A. (1999) The dependence function for bivariate extreme value
distributions, a systematic approach. Unpublished manuscript.

Rakonczai, P. (2011) Bivariate generalized Pareto distribution in practice: models and estimation. (submitted)
Available at www.cs.elte.hu/~paulo/pdf/bgpd_environmetrics.pdf.

Smith, R.L. (1990) Extreme value theory. In Handbook of Applicable Mathematics Supplement
(eds Ledermann W, Lloyd E, Vajda S and Alexander C), Wiley.

Tajvidi, N. (1996) Characterization and Some Statistical Aspects of Univariate and Multivariate
Generalized Pareto Distributions. PhD Thesis, University of Goteborg. 

Tawn, J.A. (1988) An extreme value theory model for dependent observations. \emph{Journal of
Hydrology} 101.
}

\author{
Pal Rakonczai <rakonczai.p@gmail.com>
}

\seealso{\code{\link{dbgpd}}, \code{\link{dbgpd_region}}}

\examples{
library(evd)

# wind data preparation
data(DailyWind)
demodata       = DailyWind[,c(4,6)]
nms            = names(demodata)
thr            = apply(demodata,2,quantile,prob=0.95)
potdata        = mgpd_data(demodata,thr=thr)
init           = mgpd_init(potdata)

plot(potdata[,1]+thr[1],potdata[,2]+thr[2],xlab=nms[1],ylab=nms[2],main="bgpd_data")
abline(h=thr[2],v=thr[1])

# fitting logistic model
est.log        = bgpd_fit(c(init,2),dat=potdata[,1:2],model="log",fixedmu=FALSE)
est.log        = bgpd_fit(c(init,2),dat=potdata[,1:2],model="log",fixedmu=TRUE)  

# fitting further models
est.psilog     = bgpd_fit(c(est.log$par,0),dat=potdata[,1:2],model="psilog",fixedmu=TRUE) 
est.neglog     = bgpd_fit(c(init,1),dat=potdata[,1:2],model="neglog",fixedmu=TRUE) 

# further examples below..
#est.psineglog  = bgpd_fit(c(est.neglog$par,0),dat=potdata[,1:2],model="psilog",fixedmu=TRUE)  
#est.bilog      = bgpd_fit(c(est.log$par[1:6],rep(1/est.log$par[7],2)),dat=potdata[,1:2],
#model="bilog",fixedmu=TRUE) 
#est.negbilog   = bgpd_fit(c(est.neglog$par[1:6],rep(-1/est.neglog$par[7],2)),dat=potdata[,1:2],
#model="negbilog",fixedmu=TRUE) 
#est.mix        = bgpd_fit(c(init),dat=potdata[,1:2],model="mix",fixedmu=TRUE) 
#est.ct         = bgpd_fit(c(init,1,1),dat=potdata[,1:2],model="ct",fixedmu=TRUE) 
#est.taj        = bgpd_fit(c(init,2,1),dat=potdata[,1:2],model="taj",fixedmu=TRUE) 
}
