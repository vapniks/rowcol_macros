\name{grenamecols}
\alias{grenamecols}
\title{Rename matches to regexp in column names of data.frame object.}
\usage{
  grenamecols(df = stop("df not supplied"),
    regex = stop("regex not supplied"),
    repl = stop("repl not supplied"))
}
\arguments{
  \item{df}{a data.frame object}

  \item{regex}{a string/character object}

  \item{repl}{a string/character object}
}
\value{
  none - side effects only
}
\description{
  Rename matches to regexp in column names of data.frame
  object.
}
\details{
  Replace all matches to regexp 'regex' in columns of 'df'
  with 'repl'.
}
\author{
  Ben Veal
}

