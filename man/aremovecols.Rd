\name{aremovecols}
\alias{aremovecols}
\title{Remove columns from data.frame that fuzzy match string}
\usage{
  aremovecols(df = stop("df not supplied"),
    str = stop("str not supplied"))
}
\arguments{
  \item{df}{a data.frame object}

  \item{str}{a string with which to fuzzy match the column
  names of 'df'}
}
\value{
  A subset of the columns of 'df'
}
\description{
  Uses fuzzy string matching to select which columns of
  'df' to remove.
}
\author{
  Ben Veal
}

