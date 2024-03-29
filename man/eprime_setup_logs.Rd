% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eprime_setups.R
\name{eprime_setup_logs}
\alias{eprime_setup_logs}
\title{Set up logs for eprime parser}
\usage{
eprime_setup_logs(out_dir, task, quietly = FALSE)
}
\arguments{
\item{out_dir}{directory to output the data to}

\item{task}{Which task, one of ["Antisaccade", "n-back", "Attention"]}

\item{quietly}{logical if log text should be printed in the console}
}
\value{
list of log paths
}
\description{
Initiates and creates paths to necessary
log-files for the E-prime parser
}
