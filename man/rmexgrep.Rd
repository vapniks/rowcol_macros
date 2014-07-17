\name{rmexgrep}
\alias{rmexgrep}
\title{Remove from workspace all objects with names not matching regexp}
\usage{
  rmexgrep(regexp = stop("regexp not supplied"))
}
\arguments{
  \item{regexp}{a regular expression (as a character
  object)}
}
\value{
  none - side effects only
}
\description{
  Remove from workspace all objects with names not matching
  regexp
}
\author{
  Ben Veal
}
\seealso{
  \code{\link{rmgrep}}
}

