\name{findcols}
\alias{findcols}
\title{Show names and numbers of columns from df with names matching regexp (ignoring case)}
\usage{
  findcols(df = stop("df not supplied"),
    regex = stop("regex not supplied"))
}
\arguments{
  \item{df}{a data.frame object}

  \item{regex}{a regular expression (as a character
  object)}
}
\value{
  A data.frame object with one column containing names of
  columns of 'df', and whose row names are the
  corresponding row numbers of 'df'
}
\description{
  Show names and numbers of columns from df with names
  matching regexp (ignoring case)
}

