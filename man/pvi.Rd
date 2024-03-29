% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/avfintools.R
\name{pvi}
\alias{pvi}
\title{Positive Volume Index}
\usage{
pvi(df)
}
\arguments{
\item{df}{Dataframe with price data}
}
\value{
Returns a 1 x # of columns in df dataframe
}
\description{
Calculates the  positive volume index, uses closing price of the time period
}
\examples{
pvi(tail(SPYdaily, 200))
}
