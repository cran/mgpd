\name{DailyWind}
\alias{DailyWind}
\docType{data}
\title{
German Wind Speed Data
}
\description{
Daily maxima of wind speed in m/sec at 5 locations of Northern-Germany.
}
\usage{data(DailyWind)}
\format{
  A data frame with 17926 observations on the following 6 variables.
  \describe{
    \item{\code{date}}{date of observations}
    \item{\code{Hamburg}}{a numeric vector of wind speed (m/s)}
    \item{\code{Hannover}}{a numeric vector of wind speed (m/s)}
    \item{\code{Bremerhaven}}{a numeric vector of wind speed (m/s)}
    \item{\code{Fehmarn}}{a numeric vector of wind speed (m/s)}
    \item{\code{Schleswig}}{a numeric vector of wind speed (m/s)}
  }
}

\examples{
data(DailyWind)
str(DailyWind)
}

