\name{removerows}
\alias{removerows}
\title{Remove rows from data.frame that match regexp}
\usage{
  removerows(df = stop("df not supplied"),
    regex = stop("regex not supplied"))
}
\arguments{
  \item{df}{a data.frame object}

  \item{regex}{a regular expression (as a character
  object)}
}
\value{
  A subset of the rows of 'df'
}
\description{
  Remove rows from data.frame that match regexp
}
\author{
  Ben Veal
}

