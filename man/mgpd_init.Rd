\name{mgpd_init}
\alias{mgpd_init}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initial marginal parameters for fitting multivariate Pareto distribution
}
\description{
Initial marginal parameters for fitting multivariate Pareto distribution by \code{bgpd_fit}. 
}
\usage{
mgpd_init(xdat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xdat}{
Matrix of observations. Typically obtained by \code{mgpd_data}.
}
}
\details{
Uses \code{evd} package for fitting generalized extreme value distributioin for all margins individually.
}
\value{
Vector of marginal parameters, 3 parameters (locations, scale and shape) for all margins.
}

