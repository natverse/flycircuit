\docType{data}
\name{fcidtable}
\alias{fcidtable}
\title{fcidtable - Data.frame of flycircuit identifiers}
\description{
The flycircuit database seems to have at least 3 unique
identifiers for neurons These are \itemize{ \item{idid}{ An
integere identifier which ranged from 1:16226 in the April
2011 release} \item{gene_name}{ A character identifier of
the general form "DvGlutMARCM-F003905_seg001"} \item{Name}{
A character identifier of the general form
"VGlut-F-400794"} }
}
\details{
Although the neuron \code{Name} is generally what is
reported in publications the \code{gene_name} seems to
widely used and is what connected to the confocal image
file name. The \code{_seg001} suffix in gene_names refers
to the segmentation of individiual neurons from stacks that
might contain multiple labelled neurons after MARCM.
}
\seealso{
Other flycircuit.ids: \code{\link{fc_gene_name}},
\code{\link{fc_idid}}, \code{\link{fc_neuron}}
}

