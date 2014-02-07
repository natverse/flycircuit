\name{hclustfc}
\alias{hclustfc}
\title{Cluster a set of FlyCircuit neurons identified by gene_name}
\usage{
hclustfc(gns, method = "ward", scoremat = NULL, distfun = as.dist, ...,
  maxneurons = 4000)
}
\arguments{
  \item{gns}{FlyCircuit identifiers (passed to
  fc_gene_name).}

  \item{method}{Clustering method (default Ward's).}

  \item{scoremat}{Score matrix to use (see
  \code{fc_subscoremat} for details of default)}

  \item{distfun}{Function to convert distance matrix
  returned by \code{fc_sub_distmat} into R dist object
  (default=as.dist).}

  \item{...}{Additional parameters passed to hclust.}

  \item{maxneurons}{Set this to a sensible value to avoid
  loading huge order N^2 distances directly into memory.}
}
\value{
An object of class \code{\link{hclust}} which describes the
tree produced by the clustering process.
}
\description{
Given a vector of gene/neuron names or neuronids use hclust
to carry out a hierarchical clustering. The default value
of distfun will handle square distance matrices and R.
}
\seealso{
\code{\link{fc_gene_name}}, \code{\link{hclust}},
\code{\link{dist}}

Other scoremats: \code{\link{fc_sub_distmat}}
}

