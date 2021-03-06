% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_graphing.R
\name{add_ppg_waveform}
\alias{add_ppg_waveform}
\title{Internal \code{ibiVizEdit} utility that adds PPG to plot}
\usage{
add_ppg_waveform(
  base_plot = NULL,
  ppg_data = NULL,
  show_ppg = FALSE,
  time_col = "Time",
  ppg_col = "PPG"
)
}
\arguments{
\item{base_plot}{is a \code{ggplot2} object, typically generated by \code{generate_base_gui_plot}}

\item{ppg_data}{is a \code{data.frame} that contains the processed PPG signal and a time variable.}

\item{show_ppg}{logical value based on user selection. Status governs the display of underlying PPG signal}

\item{time_col}{is of type \code{character} and is the column name in the \code{ppg_data} that contains the time
variable}

\item{ppg_col}{is of type \code{character} and is the column name in the \code{ppg_data} that contains the PPG signal}
}
\description{
Internal \code{ibiVizEdit} utility that adds PPG to plot
}
