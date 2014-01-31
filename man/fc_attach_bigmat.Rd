\name{fc_attach_bigmat}
\alias{fc_attach_bigmat}
\title{Attach a big.matrix (typically used for all-by-all blast distances).}
\usage{
fc_attach_bigmat(bigmat)
}
\arguments{
  \item{bigmat}{Name of big matrix object (which should
  match file on disk).}

  \item{return}{A big matrix object.}
}
\description{
These are file based matrices that are not loaded into
memory. If bigmat="mybigmat" there should be a big.matrix
description file called
file.path(fcconfig$bigmatrixdir,'mybigmat.desc')
}
\examples{
\dontrun{
fc_attach_bigmat()
}
}
\seealso{
\code{\link{attach.big.matrix}}
}

