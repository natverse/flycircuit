#' See a FlyCircuit neuron on the FlyCircuit website
#'
#' @description See chosen FlyCircuit neuron on the FlyCircuit website, where maximal projection .lsm images are available.
#' @param fc.ids a vectors of FlyCircuit neuron IDs
#' @param open Whether or not to open the pages in your default web browser
#' @param max Maximum number of webpages to open
#' @return the URLs (invisibly if they are being opened)
#' @source \url{http://www.flycircuit.tw/}
#' @examples 
#' \donttest{
#' # Find the details page from a neuron on the FlyCircuit database
#' fc_page("Gad1-F-200234", open=FALSE)
#' }
#' @export
#' @importFrom utils browseURL
fc_page <- function (fc.ids, open=TRUE, max = 10 ){
  if(open && length(fc.ids)>max)
    stop("Too many FlyCircuit IDs!!!")

  urls <- paste0("http://flycircuit.tw/modules.php?name=clearpage&op=detail_table&neuron=",fc.ids)
  if(open) {
    sapply(urls, utils::browseURL)
    invisible(urls)
  } else urls
}
