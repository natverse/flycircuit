\name{fc_nblast}
\alias{fc_nblast}
\title{Return (normalised) nblast similarity scores for FlyCircuit neurons}
\usage{
fc_nblast(query, target, scorematname = NULL, normalised = FALSE)
}
\arguments{
  \item{query,target}{Vectors of FlyCircuit identifiers}

  \item{scorematname}{The score matrix to use. When NULL
  will default to "abc2.normdmat" if
  \code{normalised=TRUE}, \code{allbyallblastcv2.5.bin}
  otherwise.}

  \item{normalised}{Logical indicating whether to return
  normalised scores.}

  \item{...}{A}
}
\value{
Matrix of scores, columns are query neurons, rows, target.
}
\description{
Precomputed scores are stored in bigmatrix form on disk.
The bigmatrix object is created in the global environment
once per session and then reused. When normalised is TRUE,
scores are in the interval [-1, 1] or, more exactly,
range(scorematrix). Higher is always better. NB normalised
scores are the mean of the normalised forward and reverse
scores (as used for clustering in hclustfc) but are
similarity scores (not distances).
}
\details{
See the package vignette for an examlple of how to download
a precomputed score matrix.
}
\seealso{
\code{\link{hclustfc},\link{fc_sub_distmat}}
}

