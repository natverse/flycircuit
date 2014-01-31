#' Load an rda object cached on disk into the Global Environment.
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
  rdafile <- file.path(getOption('flycircuit.localroot'), folder, paste(data, sep=".", "rda"))
  if(!file.exists(rdafile))
    stop("Unable to read file: ", rdafile)
  load(rdafile, envir=.GlobalEnv)
}
