\name{selectcolsT}
\alias{selectcolsT}
\title{Select columns from data.table matching regexp}
\usage{
  selectcolsT(dt = stop("dt not supplied"),
    regex = stop("regex not supplied"))
}
\arguments{
  \item{dt}{a data.table object}

  \item{regex}{a regular expression}
}
\value{
  A subset of the columns of 'dt'
}
\description{
  Select columns from data.table matching regexp
}
\author{
  Ben Veal
}
\seealso{
  \code{\link{selectcols}}, \code{\link{aselectcols}},
  \code{\link{aselectcolsT}}
}

