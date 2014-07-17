\name{selectrows}
\alias{selectrows}
\title{Select rows from data.frame matching regexp}
\usage{
  selectrows(df = stop("df not supplied"),
    regex = stop("regex not supplied"))
}
\arguments{
  \item{df}{a data.frame object}

  \item{regex}{a regular expression}
}
\value{
  A subset of the rows of 'df'
}
\description{
  Select rows from data.frame matching regexp
}
\author{
  Ben Veal
}

