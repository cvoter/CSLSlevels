% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_analysis_window.R
\name{plot_analysis_window}
\alias{plot_analysis_window}
\title{Plot lake levels under varying analysis windows}
\usage{
plot_analysis_window(
  df,
  xintercept = NA,
  text_size,
  lakes = c("Pleasant", "Long", "Plainfield")
)
}
\arguments{
\item{df}{data frame to plot with the end year of analysis (year), lake level
(level), exceedance probability (prob) and lake name (lake)}

\item{xintercept}{optional year to highlight with a dashed line}

\item{text_size}{size of text in plots}

\item{lakes}{the names of the lakes in the order you would like them
displayed in plots, etc. Defaults to c("Pleasant", "Long",
"Plainfield")}
}
\value{
plot_obj, the plot.
}
\description{
This function plots the lake level at three predetermined exceedance
probabilities (e.g., 10, 50, 90 percent) for a range of analysis end years at
all lakes.
}
