% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/rowcol_macros.R
\name{afindrows}
\alias{afindrows}
\title{Select rows from data.frame fuzzy matching string (ignoring case)}
\usage{
afindrows(df = stop("df not supplied"), str = stop("str not supplied"))
}
\arguments{
\item{df}{a data.frame object}

\item{str}{a string}
}
\value{
A subset of the rows of 'df'
}
\description{
Select rows from data.frame fuzzy matching string (ignoring case)
}
\details{
Uses fuzzy matching to select rows from 'df' that match 'str'
}
\author{
Ben Veal
}

