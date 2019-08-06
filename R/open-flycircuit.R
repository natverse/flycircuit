#' See a FlyCircuit neuron on the FlyCircuit website
#'
#' @description See chosen FlyCircuit neuron on the FlyCircuit website, where maximal projection .lsm images are available.
#' @param fc.ids a vectors of FlyCircuit neuron IDs
#' @param max Maximum number of webpages to open
#' @source \url{http://www.flycircuit.tw/}
#' @examples 
#' \donttest{
#' # Let's read a neuron from the FlyCircuit database
#' flycircuit_page("Gad1-F-200234")
#' }
#' @export
#' @importFrom utils browseURL
flycircuit_page <- function (fc.ids, max = 10){
  if(length(fc.ids)>10){
    stop("Too many FlyCircuit IDs!!!")
  }
  for(f in 1:length(fc.ids)){
    url <- paste0("http://flycircuit.tw/modules.php?name=clearpage&op=detail_table&neuron=",f)
    utils::browseURL(url)
  }
}