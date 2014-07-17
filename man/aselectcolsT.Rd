\name{aselectcolsT}
\alias{aselectcolsT}
\title{Select columns from data.table fuzzy matching string}
\usage{
  aselectcolsT(dt = stop("dt not supplied"),
    str = stop("str not supplied"))
}
\arguments{
  \item{dt}{a data.table object}

  \item{str}{a string}
}
\value{
  A subset of the columns of 'dt'
}
\description{
  Uses fuzzy matching to select columns from 'dt' that
  match 'str'
}
\author{
  Ben Veal
}
\seealso{
  \code{\link{selectcols}}, \code{\link{selectcolsT}},
  \code{\link{aselectcols}}
}

