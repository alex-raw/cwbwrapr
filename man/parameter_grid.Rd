% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cwb_scan_interface.R
\name{parameter_grid}
\alias{parameter_grid}
\title{Create a data grid}
\usage{
parameter_grid(corpus, ...)
}
\arguments{
\item{corpus}{character vector of corpus names in CWB registry}

\item{p_attr}{character vector of s-attributes}

\item{s_attr}{character vector of p-attributes}
}
\description{
Create a grid with combinations of parameters to scan. This
includes a currently hard-coded look-up table to replace alternative attribute names.
Depends on installation
}
