\name{fc_attach_ff}
\alias{fc_attach_ff}
\title{Attach an ff object (typically used for all-by-all blast distances)}
\usage{
fc_attach_ff(ff)
}
\arguments{
  \item{ff}{Name of ff object (which should match file on
  disk).}
}
\value{
An ff object.
}
\description{
These are file based matrices that are not loaded into
memory. If \code{ff="myff"} there should be a ff data file
called \code{'myff.ffData'}
}
\examples{
\dontrun{
fc_attach_ff()
}
}

