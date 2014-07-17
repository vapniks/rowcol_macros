\name{aselectcols}
\alias{aselectcols}
\title{Select columns from data.frame fuzzy matching string}
\usage{
  aselectcols(df = stop("df not supplied"),
    str = stop("str not supplied"))
}
\arguments{
  \item{df}{a data.frame object}

  \item{str}{a string}
}
\value{
  A subset of the columns of 'df'
}
\description{
  Uses fuzzy matching to select columns from 'df' that
  match 'str'
}
\author{
  Ben Veal
}
\seealso{
  \code{\link{selectcols}}, \code{\link{selectcolsT}},
  \code{\link{aselectcolsT}}
}

