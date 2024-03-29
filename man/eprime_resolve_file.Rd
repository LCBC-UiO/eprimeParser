% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eprime_resolve.R
\name{eprime_resolve_file}
\alias{eprime_resolve_file}
\title{Attempt manual resolve on a file that is not completed}
\usage{
eprime_resolve_file(files_date_time, file_date, in_dir, task, paths, logs,
  MOAS, gmail_user = NULL, quietly = FALSE, .choice = NULL)
}
\arguments{
\item{files_date_time}{The unique file name for file that 
needs processing (found in status.log)}

\item{file_date}{the date of the session for the file that
needs processing (found in status.log)}

\item{in_dir}{Directory with E-prime files}

\item{task}{Which task, one of ["Antisaccade", "n-back", "Attention"]}

\item{paths}{list of paths from [\code{eprime_setup_dirs}]}

\item{logs}{list of paths from [\code{eprime_setup_logs}]}

\item{MOAS}{Either string with path to, or as data.frame, the MOAS dataframe}

\item{gmail_user}{Email with access to the LCBC test calendar
on google}

\item{quietly}{logical if log text should be printed in the console}

\item{.choice}{never to be set by the user. Created for unit testing only}
}
\description{
Will attempt to process the file automatically 
before prompting for manual input to resolve the issues 
that are stopping the file from being processed.
}
