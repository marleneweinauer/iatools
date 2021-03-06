% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_iaclass.r
\name{create_iaclass}
\alias{create_iaclass}
\title{create_iaclass}
\usage{
create_iaclass(dat, variables, key_variables = NULL, min_interviews = 15, type)
}
\arguments{
\item{dat}{survey data or time stamp data. dat must be a data.table or data.frame in wide-table-format with observations in rows and variables in collumns.
The collum of IDs of observations must be named ID. The collum of interviewers must be named INTERVIEWER.}

\item{variables}{a character vector of variables you want to be tested in further tests}

\item{key_variables}{a character vector of variables of high importance e.g. gate_questions}

\item{min_intervies}{the treshold of minimum of interviews conducted by an interviewer to be analysed}
}
\value{
object of class be_iaclass
}
\description{
Most functions of the ia tools package need data to be of class be_iaclass. The create_iaclass function
transfers your data to this class.
}
\details{
The restriction of dat to a specific mode is recommended! The tests are designed for CATI- telephone interviews
}
\examples{
\dontrun{a <- 1}
}
