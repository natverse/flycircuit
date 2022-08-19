#' Load an rda/rds object cached on disk into the Global Environment
#' 
#' @details if object.rda and object.rds \strong{both} exist on disk, the 
#'   \strong{former} will take priority.
#' @details Starts by checking whether an object of the appropiate name is 
#'   already loaded in the specified environment unless \code{Force=TRUE}. Note 
#'   that only the name is inspected and there is no way to check if the object
#'   matches the one on disk implied by the call i.e. if you create a different
#'   object with a name matching one that you would like to use with
#'   \code{load_fcdata}, the object you want will be masked by the one in memory
#'   and will not be loaded.
#' @param data Name of object (and the stem of rda file).
#' @param folder Name of the project subfolder containing object.
#' @param Force Whether to load even if table already exists (default FALSE).
#' @param envir The environment into which the contents of an rda (but not rds) 
#'   file should be loaded. Defaults to \code{.GlobalEnv}, the global user 
#'   environment.
#' @param inherits Whether to check the enclosing frames of the environment 
#'   specified by \code{envir} for existence of \code{data}. See the 
#'   \code{inherits} argument of \code{\link{exists}} for details.
#' @return For rda files, a character vector of the names of objects created, 
#'   invisibly or NULL if nothing loaded. For rds files, the object itself.
#' @export
load_fcdata <- function(data, Force=FALSE, folder=c('data','db'),
                        envir=.GlobalEnv, inherits=TRUE) {
  folder <- match.arg(folder)
  dontload = exists(data, where=envir, inherits=inherits) && !is.function(get(data)) && !Force
  folder <- ifelse(folder == 'db', getOption('flycircuit.dbdir'), getOption('flycircuit.datadir')) 
  rdafile <- file.path(folder, paste(data, sep=".", "rda"))
  if(file.exists(rdafile)) {
    if(dontload) return(invisible(NULL)) else {
      loadres=load(rdafile, envir=envir)
      if(length(loadres)!=1)
        warning("load_fcdata is not recommended when > 1 object per rda file!")
      if(loadres!=data)
        warning("load_fcdata expects that object name matches file name!")
      return(invisible(loadres))
    } 
  }
  rdsfile <- file.path(folder, paste(data, sep=".", "rds"))
  if(!file.exists(rdsfile))
    stop("Neither ", rdafile,' nor ',rdsfile,' exists!')
  if(dontload) return(get(data)) else readRDS(file=rdsfile)
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
  if(tools::file_ext(bigmat)=='ff') {
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
#' @import ff
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
    attr(attr(ffobj, 'physical'), 'filename') <- path.expand(paste0(getOption('flycircuit.ffdir'), '/', ff, '.ff'))
    assign(ff, ffobj, envir=envir)
  }
  invisible(get(ff, envir=envir))
}

#' Download a data file from a remote location
#' 
#' By default this function will check for an up to date local version of the 
#' remote file using the \code{\link{download.file.wcheck}} function.
#' 
#' @details The default value for the \code{overwrite} argument can be changed 
#'   by setting \code{options('flycircuit.remote_overwrite')} to \code{TRUE} or 
#'   \code{FALSE}. Setting this to \code{FALSE} may be useful if checking the 
#'   headers of the remote files takes too long.
#'   
#' @param url the location of the remote file.
#' @param type the type of file (data, db, or bigmat).
#' @param overwrite whether to overwrite an existing file. If \code{NULL} (the 
#'   default package option), the HTTP headers are inspected to see if the 
#'   remote version is newer than the local version and only downloads the 
#'   remote version if this is so.
#' @param ... additional arguments passed to download.file (e.g. quiet).
#' @return The path to the downloaded file.
#'   
#' @importFrom RCurl url.exists
#' @export
#' @seealso \code{\link{download.file.wcheck}}
#' @examples
#' \dontrun{
#' fc_download_data("http://myurl.com/data", quiet=TRUE)
#' fc_download_data("http://myurl.com/data.ff", type='ff')
#' 
#' fc_download_data("http://myurl.com/data.desc", type='bigmat')
#' }
fc_download_data <- function(url, type=c('data', 'db', 'bigmat', 'ff'),
                             overwrite=getOption('flycircuit.remote_overwrite'), ...) {
  folder <- match.arg(type)
  folderpath <- switch(folder,
    'data' = getOption('flycircuit.datadir'),
    'db' = getOption('flycircuit.dbdir'),
    'bigmat' = getOption('flycircuit.bigmatdir'),
    'ff' = getOption('flycircuit.ffdir')
  )
  
  rval=download.file.wcheck(url, destdir=folderpath, ...)
  
  # If we've been given the URL for a bigmat .desc file, also download the bigmat
  if(folder == 'bigmat') {
    bigmaturl=sub("[.][^.]*$", "", url, perl=T)
    if(url.exists(bigmaturl)){
      rval=c(rval,download.file.wcheck(bigmaturl, destdir=folderpath, ...))
    } else {
      # try xxx.bin as well if that didn't exist
      bigmaturl=paste0(bigmaturl,'.bin')
      rval=c(rval,download.file.wcheck(bigmaturl, destdir=folderpath, ...))
    }
    
  } else if(folder == 'ff') {
    # If we've been given the URL for a .ff file, also download the .ffrds file
    ffurl <- paste0(sub("[.][^.]*$", "", url, perl=T), '.ffrds')
    rval=c(rval, download.file.wcheck(ffurl, destdir=folderpath, ...))
  }
  
  rval
}

#' Download a remote file if it is new or updated
#' 
#' Wraps regular \code{\link{download.file}}, but by default caches the the etag
#' in the file header and then compares this before downloading again.
#' 
#' @details The header will be cached in a file called 
#'   \code{XXXX.http_header.rds} next to the downloaded file.
#'   
#'   When the URL is unreachable, a check is made to see if the corresponding 
#'   file already exists locally. If yes, then a warning is issued, but that 
#'   path is used. If no, then the function will error out.
#' @param url Location of remote file to download
#' @param destdir Path to local download directory
#' @param destfile Full path to local downloaded file
#' @param overwrite Whether to check etag (\code{NULL}) or only download if file
#'   is missing (\code{FALSE}) or always download (\code{TRUE}).
#' @param mode See the \code{\link{download.file}} argument of the same name.
#'   Must be set to \code{'wb'} on Windows to download binary files and
#'   therefore defaults to this value.
#' @param ... Additional arguments passed to \code{download.file}
#' @seealso \code{\link{download.file}}
#' @details The header will be cached in a file called XXXX.http_header.rds next
#'   to the downloaded file
#' @return Path to downloaded (or cached) file.
#' @export
#' @importFrom utils download.file
download.file.wcheck<-function(url, destdir=NULL, destfile=NULL, overwrite=NULL,
                               mode='wb', ...){
  if(is.null(destdir) && is.null(destfile)) stop("Must specify one of destdir or destfile")
  if(is.null(destfile)) destfile=file.path(destdir, basename(url))
  
  rval = try({
    if(is.null(overwrite)){
      http_header <- c(url.exists(url, .header = T), url)
      if(isTRUE(http_header[[1]]==FALSE) || !identical(http_header[['statusMessage']],"OK")){
        stop("Unable to read URL: ", url)
      }
      overwrite <- TRUE
      
      header_file <- paste0(destfile, '.http_header.rds')
      if(file.exists(header_file)) {
        http_file_header <- readRDS(header_file)
        overwrite <- !identical(http_header['ETag'], http_file_header['ETag'])
      }
      on.exit(saveRDS(http_header, file=header_file, compress='xz'))
    }
      
    if(!overwrite && file.exists(destfile))
      message("Using cached version of file.")
    else download.file(url, destfile=destfile, mode=mode, ...)
  }, silent=T)
  
  if(inherits(rval, 'try-error')){
    if(!file.exists(destfile))
      stop("Unable to read url: ",url," and no cached file exists!")
    warning("Unable to read url: ",url,". Using cached file: ", destfile)
  }

  invisible(destfile)
}


#' Load NBLAST supplemental data objects, downloading when required
#' 
#' \code{load_si_data} first checks to see if file must be downloaded from the 
#' jefferislab.org website (because it is missing or out of date). It then loads
#' the file into the current R session.
#' 
#' @details Note that \code{bigmat} and \code{ff} objects will be loaded using 
#'   the function \code{\link{fc_attach_bigmat}}.
#' @param data_name the name of the file to load.
#' @param type either \code{auto}, stating that the file should be handled 
#'   automagically, \code{plain}, specifying that the file should just be 
#'   downloaded, or the type of file (data, db, ff, or bigmat).
#' @param overwrite whether to overwrite an existing file (default: 
#'   \code{FALSE}). See \code{\link{fc_download_data}}.
#' @param ... extra arguments to pass to \code{\link{fc_download_data}}.
#'   
#' @return For rda files, a character vector of the names of objects created, 
#'   invisibly or \code{NULL} if nothing loaded. For rds, bigmat or ff files, 
#'   the object itself. Otherwise, the path to the downloaded file.
#' @export
#' @seealso \code{\link{fc_download_data}}, \code{\link{fc_attach_bigmat}}
#' @examples 
#' \dontrun{
#' mydata=load_si_data("mydata.rds")
#' # note that rda files are loaded into the global environment.
#' load_si_data("mydata.rda")
#' bigmat=load_si_data("data.desc")
#' }
load_si_data <- function(data_name, type=c('auto', 'data', 'db', 'bigmat', 'ff', 'plain'),
                         overwrite=FALSE, ...) {
  type <- match.arg(type)
  if(type == "auto") {
    if(grepl("\\.rd[as]$", data_name)) type <- "data"
    else if(grepl("\\.ff(rds)*$", data_name)) type <- "ff"
    else if(grepl("\\.desc$", data_name)) type <- "bigmat"
    else type <- "plain"
  }
  filepath <- fc_download_data(flycircuit.sidataurl(data_name), overwrite=overwrite, type=ifelse(type=="plain", "data", type), ...)
  
  if(type == "plain") {
    filepath
  } else if(type == "data" || type == "db") {
    data_name <- gsub("\\.rd[as]$", "", data_name)
    load_fcdata(data_name)
  } else if(type == "bigmat" || type == "ff") {
    data_name <- gsub("\\.desc$", "", data_name)
    fc_attach_bigmat(data_name)
  }
}


flycircuit.sidataurl_memo <- memoise::memoise(function() {
  res=getOption('flycircuit.sidataurl')
  if(is.null(res))
    stop("Please set options(flycircuit.sidataurl)!")
  for(u in res) {
    if(my_url_ok(u)) return(u)
    warning("Unable to reach SI data URL: ", u)
  }
  stop("Unable to reach any SI data URLs!\n",
       "Ask on https://groups.google.com/g/nat-user for help.")
}, ~memoise::timeout(3600))

# private function to return full URL to SI data
flycircuit.sidataurl <- function(...) {
  baseurl=flycircuit.sidataurl_memo()
  file.path(baseurl, ..., fsep="/")
}

my_url_ok <- function(url, timeout=4) {
  stat=try(httr::status_code(httr::HEAD(url, httr::timeout(timeout))), silent = T)
  identical(stat, 200L)
}

# Not exported for the moment - utility function to upload si data to server
upload_si_data<-function(x, format=c('rds', 'rda'),
                         target="lmbfly:/var/www/html/si/nblast/flycircuit") {
  xname=deparse(substitute(x))
  format=match.arg(format)
  td=tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = T))
  rdfile=file.path(td, paste0(xname, '.', format))
  if(format=='rds'){
    saveRDS(x, file=rdfile)
  } else {
    save(list=xname, file=rdfile)
  }
  scp_upload(rdfile, target=target)
}

scp_upload<-function (localpath, user, host, remotepath, target) 
{
  if (missing(target)) 
    target = sprintf("%s@%s:%s", user, host, remotepath)
  system(paste("scp", shQuote(path.expand(localpath)), target))
}
