\name{removecolsT}
\alias{removecolsT}
\title{Remove columns from data.table that match regexp}
\usage{
  removecolsT(dt = stop("dt not supplied"),
    regex = stop("regex not supplied"))
}
\arguments{
  \item{dt}{a data.table object}

  \item{regex}{a regular expression (as a character
  object)}
}
\value{
  A subset of the columns of 'dt'
}
\description{
  Remove columns from data.table that match regexp
}
\author{
  Ben Veal
}

