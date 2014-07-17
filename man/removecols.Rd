\name{removecols}
\alias{removecols}
\title{Remove columns from data.frame that match regexp}
\usage{
  removecols(df = stop("df not supplied"),
    regex = stop("regex not supplied"))
}
\arguments{
  \item{df}{a data.frame object}

  \item{regex}{a regular expression (as a character
  object)}
}
\value{
  A subset of the columns of 'df'
}
\description{
  Remove columns from data.frame that match regexp
}
\author{
  Ben Veal
}

