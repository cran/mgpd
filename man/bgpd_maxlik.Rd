\name{bgpd_maxlik}
\alias{bgpd_maxlik}
\title{
Log-likelihood function for bivariate generalized Pareto distribution
}
\description{
Evaluates the log-likelihood for BGPD.
}
\usage{
bgpd_maxlik(param, dat, model="log", fixed=FALSE, ...)
}
\arguments{
  \item{param}{
Vector of parameters to be evaluated over.
}
  \item{dat}{
Matrix of observations (with two coloumns).
}
  \item{model}{
Parametric model for the underlying dependence structure. The \emph{log},\emph{neglog},\emph{taj}, \emph{mix} are symmetric and 
\emph{psilog},\emph{psineglog},\emph{bilog},\emph{negbilog},\emph{ct} are asymmetric models.
}
 \item{fixed}{
If TRUE then the location parameter of the x margin is kept to be 0.
}
 \item{...}{
Further arguments to be passed.
}
}
\details{
Internal function.
}
\author{
Pal Rakonczai <rakonczai.p@gmail.com>
}