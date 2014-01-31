#' flycircuit
#'
#' @name flycircuit
#' @docType package
NULL

#' fcidtable - Data.frame of flycircuit identifiers
#' 
#' The flycircuit database seems to have at least 3 unique identifiers for neurons
#' These are
#' \itemize{
#' \item{idid}{ An integere identifier which ranged from 1:16226 in the April
#' 2011 release}
#' \item{gene_name}{ A character identifier of the general form
#' "DvGlutMARCM-F003905_seg001"}
#' \item{Name}{ A character identifier of the general form "VGlut-F-400794"}
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

# Set local directory for data storage and remote location for data download
.onLoad <- function(libname, pkgname) {
  options('flycircuit.localroot' = system.file(package='flycircuit'))
  options('flycircuit.datadir' = 'data')
  options('flycircuit.dbdir' = 'db')
  options('flycircuit.bigmatdir' = file.path(getOption('flycircuit.datadir'), 'bigmat'))
  options('flycircuit.remoteloc' = 'http://flybrain.mrc-lmb.cam.ac.uk/flycircuit/')
}

.onAttach <- function(libname, pkgname) {
  options('nat.default.neuronlist' = 'dps')
}
