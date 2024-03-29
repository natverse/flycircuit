% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flycircuit-package.r
\docType{package}
\name{flycircuit-package}
\alias{flycircuit}
\alias{flycircuit-package}
\title{Support functions for analysis of raw image data provided by flycircuit.tw}
\description{
Functions and data for spatial analysis and annotation of neurons from the 'FlyCircuit' collection available online at http://flycircuit.tw.
}
\section{Additional data}{
 You can find out more about the data processing
  that we carried out by visiting the companion website for the NBLAST paper
  \url{https://bit.ly/flybrains}. This includes detailed instructions for the
  \href{http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/www/nblast_desktop/#data}{associated
  data downloads}.
}

\section{Package options}{


  flycircuit uses the following \code{\link{options}} to configure behaviour:

  \itemize{

  \item \code{flycircuit.datadir}: path to directory where downloaded data
  will be stored. Defaults to package's \code{extdata} folder.

  \item \code{flycircuit.dbdir}: location to which dataframes caching
  database tables will be downloaded.

  \item \code{flycircuit.ffdir}: location of \code{ff} objects used for all
  by all score matrices.

  \item \code{flycircuit.bigmatdir}: location of \code{big.matrix} objects
  previously used for all by all score matrices.

  \item \code{flycircuit.scoremat}: character vector naming the default
  regular or \code{ff} matrix object containing nblast scores. Note that
  although it is is permissible to set this to the actual score matrix rather
  than a character vector naming the matrix, this turns out to have
  substantial performance penalties for large matrices.

  \item \code{flycircuit.remote_overwrite}: Determines how local cached
  versions of data available on the web are updated. When \code{NULL} (i.e.
  unset) the web version is checked and, if it is newer, it is donwloaded and
  used to overwrite the cached copy. When \code{FALSE} the exisiting local
  copy is used; when \code{TRUE} the remote version is always downloaded.

  }
}

\examples{
# Show state of flycircuit package options
options()[grep('^flycircuit', names(options()))]

}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/natverse/flycircuit}
  \item \url{https://natverse.github.io/flycircuit}
  \item \url{http://flycircuit.tw}
  \item Report bugs at \url{https://github.com/natverse/flycircuit/issues}
}

}
\author{
\strong{Maintainer}: Gregory Jefferis \email{jefferis@gmail.com} (\href{https://orcid.org/0000-0002-0587-9355}{ORCID})

Authors:
\itemize{
  \item James Manton (\href{https://orcid.org/0000-0001-9260-3156}{ORCID})
}

Other contributors:
\itemize{
  \item Alexander Bates \email{alexander.shakeel.bates@gmail.com} (\href{https://orcid.org/0000-0002-1195-0445}{ORCID}) [contributor]
}

}
\keyword{internal}
