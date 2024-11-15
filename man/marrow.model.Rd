% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/marrow.model.R
\name{marrow.model}
\alias{marrow.model}
\title{Model generation}
\usage{
marrow.model(hf, geyr = 15, geysat = 2, all = TRUE)
}
\arguments{
\item{hf}{A hyperframe}

\item{geyr}{Geyer r variable (radius)}

\item{geysat}{Geyer saturation variable}

\item{all}{If TRUE then all models made. If FALSE only the basic model.}
}
\value{
Gibb's Geyer type model of the bone marrow from hyperframe of data.
}
\description{
Model generation
}
\examples{
marrow.model(hf, geyr = 15, geysat = 2, all = TRUE)
}
