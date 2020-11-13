#' Support functions for analysis of raw image data provided by flycircuit.tw
#' 
#' @name flycircuit
#' @description Support functions for analysis of raw image data provided by flycircuit.tw: read neurons,
#' read their IDs, summon the relevant flyciruit web-pages, get hold of precomputed flycircuit data.
#' @docType package
#' @section Package options:
#'   
#'   flycircuit uses the following \code{\link{options}} to configure behaviour:
#'   
#'   \itemize{
#'   
#'   \item \code{flycircuit.datadir}: path to directory where downloaded data 
#'   will be stored. Defaults to package's \code{extdata} folder.
#'   
#'   \item \code{flycircuit.dbdir}: location to which dataframes caching 
#'   database tables will be downloaded.
#'   
#'   \item \code{flycircuit.ffdir}: location of \code{ff} objects used for all 
#'   by all score matrices.
#'   
#'   \item \code{flycircuit.bigmatdir}: location of \code{big.matrix} objects 
#'   previously used for all by all score matrices.
#'   
#'   \item \code{flycircuit.scoremat}: character vector naming the default 
#'   regular or \code{ff} matrix object containing nblast scores. Note that 
#'   although it is is permissible to set this to the actual score matrix rather
#'   than a character vector naming the matrix, this turns out to have 
#'   substantial performance penalties for large matrices.
#'   
#'   \item \code{flycircuit.remote_overwrite}: Determines how local cached 
#'   versions of data available on the web are updated. When \code{NULL} (i.e. 
#'   unset) the web version is checked and, if it is newer, it is donwloaded and
#'   used to overwrite the cached copy. When \code{FALSE} the exisiting local 
#'   copy is used; when \code{TRUE} the remote version is always downloaded.
#'   
#'   }
#' @seealso \code{\link{fc_download_data}}
#' @references \url{http://flycircuit.tw}
#' @import nat rgl
#' @examples
#' # Show state of flycircuit package options
#' options()[grep('^flycircuit', names(options()))]
NULL
