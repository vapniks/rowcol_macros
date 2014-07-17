\name{aselectrows}
\alias{aselectrows}
\title{Select rows from data.frame fuzzy matching string}
\usage{
  aselectrows(df = stop("df not supplied"),
    str = stop("str not supplied"))
}
\arguments{
  \item{df}{a data.frame object}

  \item{str}{a string}
}
\value{
  A subset of the rows of 'df'
}
\description{
  Select rows from data.frame fuzzy matching string
}
\details{
  Uses fuzzy matching to select rows from 'df' that match
  'str'
}
\author{
  Ben Veal
}

