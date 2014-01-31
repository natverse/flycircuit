#' Load an rda object cached on disk into the Global Environment
#'
#' @param data Name of object (and the stem of rda file).
#' @param folder Name of the project subfolder containing object.
#' @param Force Whether to load even if table already exists (default FALSE).
#' @return A character vector of the names of objects created, invisibly
#'  or NULL if nothing loaded.
#' @export
load_fcdata <- function(data, Force=FALSE, folder=c('data','db')) {
  folder <- match.arg(folder)
  if(exists(data) && !is.function(get(data)) && !Force)
    return(NULL)
  folder <- ifelse(folder == 'db', getOption('flycircuit.dbdir'), getOption('flycircuit.datadir')) 
  rdafile <- file.path(getOption('flycircuit.localroot'), folder, paste(data, sep=".", "rda"))
  if(!file.exists(rdafile))
    stop("Unable to read file: ", rdafile)
  load(rdafile, envir=.GlobalEnv)
}

#' Load a database cached in the db subfolder into the Global Environment
#'
#' @param db Name of table (and the stem of rda file).
#' @param Force Whether to load even if table already exists (default FALSE).
#' @return A character vector of the names of objects created, invisibly
#'  or NULL if nothing loaded.
#' @export
#' @examples
#' load_fcdb("neuron")
#' load_fcdb(neuron) # also works
#' load_fcdb(neuron,Force=TRUE)
load_fcdb <- function(db, Force=FALSE, ...) {
  db <- as.character(substitute(db))
  load_fcdata(db, Force=Force, folder='db', ...)
}

#' Attach a big.matrix (typically used for all-by-all blast distances)
#'
#' These are file based matrices that are not loaded into memory. If
#' \code{bigmat="mybigmat"} there should be a big.matrix description file called 
#' \code{file.path(fcconfig$bigmatrixdir,'mybigmat.desc')}
#' @param bigmat Name of big matrix object (which should match file on disk).
#' @param return A big matrix object.
#' @export
#' @seealso \code{\link{attach.big.matrix}}
#' @examples
#' \dontrun{
#' fc_attach_bigmat()
#' }
fc_attach_bigmat <- function(bigmat) {
  if(!require(bigmemory))
    stop("Please install bigmemory package!")
  if(!exists(bigmat)) {
    bigmatfile <- file.path(getOption('flycircuit.localroot'), getOption('flycircuit.bigmatdir'), paste(bigmat, ".desc", sep=""))
    if(!file.exists(bigmatfile))
      stop("Cannot find file: ", bigmatfile)
    message("attaching: ", bigmat)
    assign(bigmat, attach.big.matrix(bigmatfile), envir=.GlobalEnv)
  }
  get(bigmat, envir=.GlobalEnv)
}
