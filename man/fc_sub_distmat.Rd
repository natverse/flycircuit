\name{fc_sub_distmat}
\alias{fc_sub_distmat}
\title{Return a subset of a distance matrix stored in a file-backed matrix}
\usage{
fc_sub_distmat(gns, distmat = "abc2.normdmat", form = c("matrix", "dist"),
  maxneurons = NA)
}
\arguments{
  \item{gns}{FlyCircuit identifiers (passed to
  fc_gene_name).}

  \item{distmat}{The distance matrix (default
  allbyallblast.canon).}

  \item{form}{The type of object to return.}
}
\value{
return An object of class matrix or dist (as determined by
the form argument), corresponding to a subset of the
distance matrix
}
\description{
Return a subset of a distance matrix stored in a
file-backed matrix
}

