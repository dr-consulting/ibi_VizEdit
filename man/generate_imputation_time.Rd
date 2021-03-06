% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_gp_impute_core.R
\name{generate_imputation_time}
\alias{generate_imputation_time}
\title{Internal utility for defining time variable over which to impute}
\usage{
generate_imputation_time(
  ppg_data = NULL,
  time_min = NULL,
  time_max = NULL,
  ds = NULL,
  time_col = "Time"
)
}
\arguments{
\item{ppg_data}{\code{data.frame} that contains the PPG data}

\item{time_min}{lower boundary of the time window that defines the PPG data selected for imputation}

\item{time_max}{upper boundary of the time window that defines the PPG data selected for imputation}

\item{ds}{downsampled sampling rate in Hz}

\item{time_col}{name of the column in the PPG \code{data.frame} that contains timing information in seconds}
}
\value{
the time values corresponding to the imputation window
}
\description{
Internal utility for defining time variable over which to impute
}
