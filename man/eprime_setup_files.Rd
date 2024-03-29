% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eprime_setups.R
\name{eprime_setup_files}
\alias{eprime_setup_files}
\title{Setup the raw files in the system}
\usage{
eprime_setup_files(ff_df, in_dir, task, logs, paths, quietly = FALSE)
}
\arguments{
\item{ff_df}{data frame of files to process from status.log}

\item{in_dir}{Directory with E-prime files}

\item{task}{Which task, one of ["Antisaccade", "n-back", "Attention"]}

\item{logs}{list of paths from [\code{eprime_setup_logs}]}

\item{paths}{list of paths from [\code{eprime_setup_dirs}]}

\item{quietly}{logical if log text should be printed in the console}
}
\description{
Initiates logs for each new file, and
copies the raw data into the folder
structure
}
