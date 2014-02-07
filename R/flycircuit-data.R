#' fcidtable - Data.frame of flycircuit identifiers
#' 
#' The flycircuit database seems to have at least 3 unique identifiers for neurons.
#' These are:
#' \itemize{
#' \item{idid}{An integer identifier which ranged from 1:16226 in the April
#' 2011 release}
#' \item{gene_name}{A character identifier of the general form
#' "DvGlutMARCM-F003905_seg001"}
#' \item{Name}{A character identifier of the general form "VGlut-F-400794"}
#' }
#' 
#' Although the neuron \code{Name} is generally what is reported in publications
#' the \code{gene_name} seems to widely used and is what connected to the 
#' confocal image file name. The \code{_seg001} suffix in gene_names refers to 
#' the segmentation of individiual neurons from stacks that might contain
#' multiple labelled neurons after MARCM.
#'
#' @name fcidtable
#' @family flycircuit-ids
#' @docType data
NULL

#' FCWB.surf - Surface model of FCWB FlyCircuit reference brain
#' 
#' The FCWB reference brain is a shape averaged template brain generated using 
#' the CMTK avg_adm tool. See
#' \url{https://github.com/jefferislab/MakeAverageBrain} for the relevant code.
#' 
#' @name FCWB.surf
#' @docType data
NULL

#' FCWBNP.surf - Surface model of FlyLight neuropil segmentation in FlyCircuit
#' coordinate space
#' 
#' @name FCWBNP.surf
#' @docType data
NULL

#' A dataframe containing manual annotations of some FlyCircuit neurons
#' 
#' @details \code{neuron_idid} matches \code{idid} of fcidtable
#' @seealso \code{\link{fcidtable}}
#' @name annotation
#' @docType data
NULL

#' A matrix containing all by all nblast similarity scores for 20 neurons
#' 
#' These are the same neurons whose morphology is present in the nat
#' \code{kcs20} neuronlist object.
#' @seealso \code{\link[nat]{kcs20}}
#' @name kcs20scores
#' @docType data
NULL
