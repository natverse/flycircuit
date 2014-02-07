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
  rdafile <- file.path(folder, paste(data, sep=".", "rda"))
  if(!file.exists(rdafile))
    stop("Unable to read file: ", rdafile)
  load(rdafile, envir=.GlobalEnv)
}

#' Load a database cached in the db subfolder into the Global Environment
#'
#' @param db Name of table (and the stem of rda file).
#' @param Force Whether to load even if table already exists (default FALSE).
#' @param ... Additional arguments passed to load_fcdata
#' @return A character vector of the names of objects created, invisibly
#'  or NULL if nothing loaded.
#' @export
#' @seealso load_fcdata
#' @examples
#' \dontrun{
#' load_fcdb("neuron")
#' load_fcdb(neuron) # also works
#' load_fcdb(neuron,Force=TRUE)
#' }
load_fcdb <- function(db, Force=FALSE, ...) {
  db <- as.character(substitute(db))
  load_fcdata(db, Force=Force, folder='db', ...)
}

#' Attach a big.matrix (typically used for all-by-all blast distances)
#' 
#' These are file based matrices that are not loaded into memory. If 
#' \code{bigmat="mybigmat"} there should be a big.matrix description file called
#' \code{file.path(fcconfig$bigmatrixdir,'mybigmat.desc')}. If passed a name 
#' ending in \code{'.ff'}, the corresponding ff object is attached.
#' @param bigmat Name of big matrix object (which should match file on disk).
#' @param envir The environment in which the resultant object should be created.
#' @param force Whether to overwrite an existing object of the same name
#' @return A \code{big.matrix} or \code{ff} object.
#' @export
#' @importFrom bigmemory attach.big.matrix
#' @seealso \code{\link{attach.big.matrix}}
#' @examples
#' \dontrun{
#' fc_attach_bigmat()
#' }
fc_attach_bigmat <- function(bigmat, envir=NULL, force=FALSE) {
  if(is.null(envir)) envir=.extdata
  # Check to make sure we haven't been given an ff object
  if(tail(strsplit(bigmat, "\\.")[[1]], n=1) == 'ff') {
    ffname <- sub("[.][^.]*$", "", bigmat, perl=T)
    return(fc_attach_ff(ffname))
  } else if(!exists(bigmat, where=envir)) {
    bigmatfile <- file.path(getOption('flycircuit.bigmatdir'), paste(bigmat, ".desc", sep=""))
    if(!file.exists(bigmatfile))
      stop("Cannot find file: ", bigmatfile)
    message("attaching: ", bigmat)
    assign(bigmat, attach.big.matrix(bigmatfile), envir=envir)
  }
  invisible(get(bigmat, envir=envir))
}

#' Attach an ff object (typically used for all-by-all blast distances)
#'
#' These are file based matrices that are not loaded into memory. If
#' \code{ff="myff"} there should be a ff data file called 
#' \code{'myff.ffData'}
#' @param ff Name of ff object (which should match file on disk).
#' @inheritParams fc_attach_bigmat
#' @return An ff object.
#' @export
#' @examples
#' \dontrun{
#' fc_attach_ff()
#' }
fc_attach_ff <- function(ff, envir=NULL, force=FALSE) {
  if(is.null(envir)) envir=.extdata
  if(force || !exists(ff, where=envir)) {
    fffile <- file.path(getOption('flycircuit.ffdir'), paste0(ff, '.ffrds'))
    if(!file.exists(fffile))
      stop("Cannot find file: ", fffile)
    message("attaching: ", ff)
    ffobj <- readRDS(fffile)
    # Correct path to backing file
    attr(attr(ffobj, 'physical'), 'filename') <- paste0(getOption('flycircuit.ffdir'), '/', ff, '.ff')
    assign(ff, ffobj, envir=envir)
  }
  invisible(get(ff, envir=envir))
}

#' Download a data file from a remote location
#' 
#' @details the \code{update} argument could be more intelligently implemented 
#'   e.g. by comparing the etag reported by the url header to see if the file
#'   has changed.
#' @param url The location of the remote file
#' @param type The type of file (data, db, or bigmat)
#' @param update Whether to overwrite an existing file (default: FALSE)
#' @param ... Additional arguments passed to download.file (e.g. quiet)
#' @export
#' @seealso \code{\link{download.file}}
#' @examples
#' \dontrun{
#' fc_download_data("http://myurl.com/data", quiet=TRUE)
#' fc_download_data("http://myurl.com/data.ff", type='ff')
#' }
fc_download_data <- function(url, type=c('data', 'db', 'bigmat', 'ff'), update=FALSE, ...) {
  folder <- match.arg(type)
  folderpath <- switch(folder,
    'data' = getOption('flycircuit.datadir'),
    'db' = getOption('flycircuit.dbdir'),
    'bigmat' = getOption('flycircuit.bigmatdir'),
    'ff' = getOption('flycircuit.ffdir')
  )
  
  download.file.wcheck(url, destfile=file.path(folderpath, basename(url)), ...,
                       overwrite=update)
  # If we've been given the URL for a bigmat .desc file, also download the bigmat
  if(folder == 'bigmat') {
    bigmaturl=sub("[.][^.]*$", "", url, perl=T)
    download.file.wcheck(bigmaturl, destfile=file.path(folderpath, basename(bigmaturl)),
                         ..., overwrite=update)
  }
  # If we've been given the URL for a .ff file, also download the .ffrds file
  if(folder == 'ff') {
    ffurl <- paste0(sub("[.][^.]*$", "", url, perl=T), '.ffrds')
    download.file.wcheck(ffurl, destfile=file.path(folderpath, basename(ffurl)), 
                         ..., overwrite=update)
  }
}

# utility function to download a file if not already present
# wraps regular download file
download.file.wcheck<-function(url, destfile, overwrite=FALSE, ...){
  if(!overwrite && file.exists(destfile)){
    return(TRUE)
  }
  download.file(url, destfile, ...)
}

