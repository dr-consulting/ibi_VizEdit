% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_input_and_process.R
\name{filter_ppg}
\alias{filter_ppg}
\title{Internal utility that attempts to maximize raw signal properties to generate more reliable peak locations}
\usage{
filter_ppg(ppg_data, sampling_rate, ppg_col = "PPG", time_col = "Time")
}
\arguments{
\item{ppg_data}{\code{data.frame} that contains the processed PPG data}

\item{sampling_rate}{the original sampling rate in Hz of the PPG data}

\item{ppg_col}{column name in \code{ppg_data} that contains the PPG signal}

\item{time_col}{column name in \code{ppg_data} that contains the timing information corresponding with the PPG
signal}
}
\value{
a processed and filtered PPG signal - de-noised and with linear trend removed
}
\description{
Internal utility that attempts to maximize raw signal properties to generate more reliable peak locations
}
