\name{selectcols}
\alias{selectcols}
\title{Select columns from data.frame matching regexp}
\usage{
  selectcols(df = stop("df not supplied"),
    regex = stop("regex not supplied"))
}
\arguments{
  \item{df}{a data.frame object}

  \item{regex}{a regular expression}
}
\value{
  A subset of the columns of 'df'
}
\description{
  Select columns from data.frame matching regexp
}
\author{
  Ben Veal
}
\seealso{
  \code{\link{selectcolsT}}, \code{\link{aselectcols}},
  \code{\link{aselectcolsT}}
}

