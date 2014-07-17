\name{findrows}
\alias{findrows}
\title{Show names and numbers of rows from data.frame with names matching regexp (ignoring case)}
\usage{
  findrows(df = stop("df not supplied"),
    regex = stop("regex not supplied"))
}
\arguments{
  \item{df}{a data.frame object}

  \item{regex}{a regular expression (as a character
  object)}
}
\value{
  A data.frame object with one row containing names of rows
  of 'df', and whose row names are the corresponding row
  numbers of 'df'
}
\description{
  Show names and numbers of rows from data.frame with names
  matching regexp (ignoring case)
}
\author{
  Ben Veal
}

