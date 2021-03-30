% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cqp_interface.R
\name{query_cqp}
\alias{query_cqp}
\title{Query CQP from R}
\usage{
query_cqp(query, corpus = NULL, filename)
}
\arguments{
\item{query}{character string with the cqp commands to be used
if multiple commands are stringed together, \verb{;} has to be used as separator}

\item{corpus}{character string; optionally provide corpus to be activated}

\item{filename}{name of the file to save the results to}
}
\description{
A convenience wrapper generating \code{system()} calls to cqp -c
Requires a working installation of cwb
}
\examples{
cqp_query <- '
"example"
'
filename <- "lol"
query_cqp(cqp_query, "BROWN", "lol_a_file.txt")
}