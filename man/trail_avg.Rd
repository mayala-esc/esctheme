% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esc_functions.R
\name{trail_avg}
\alias{trail_avg}
\title{Trail average
Calculates a trailing average}
\usage{
trail_avg(x, p = 4, ivars = unique(x$variable))
}
\arguments{
\item{x}{data in long format. Data in 3 columns (Dates, variable and value)}

\item{p}{Number of periods to calculate trailing average over (defaults to 4)}

\item{ivars}{The variables you want to transform. Defaults to transforming all variables.}
}
\value{
Transformed data, still in long format
}
\description{
Trail average
Calculates a trailing average
}
