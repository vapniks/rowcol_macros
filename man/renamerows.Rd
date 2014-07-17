\name{renamerows}
\alias{renamerows}
\title{Rename matches to regexp in row names of data.frame object.}
\usage{
  renamerows(df = stop("df not supplied"),
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
  Rename matches to regexp in row names of data.frame
  object.
}
\details{
  Replace first match to regexp 'regex' in rows of 'df'
  with 'repl'.
}
\author{
  Ben Veal
}

