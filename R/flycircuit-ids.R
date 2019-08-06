#' Scrape FlyCircuit.tw for neuron IDs 
#'
#' @description Scrape neuron IDs from \url{http://www.flycircuit.tw/}, which may then
#' be read into R using \code{\link{flycircuit_read_neurons}}. Note, this function 
#' can be quite slow to run.
#'
#' @param url webpage of FlyCircuit neuron IDs to scrape
#' 
#' @examples 
#' \donttest{
#' # Let's get all the FlyCircuit neurons we can
#' fc.ids = flycircuit_get_ids()
#' fcns <- flycircuit_read_neurons(fc.ids)
#' plot3d(fcns)
#' plot3d(FCWB, alpha = 0.1)
#' }
#' @source \url{http://www.flycircuit.tw/}
#' @seealso \code{\link{flycircuit_read_neurons}}, \code{\link{flycircuit_page}}
#' @return A vector of all FlyCircuit IDs
#' @export
flycircuit_get_ids <-  function(url = 'http://www.flycircuit.tw/modules.php?name=browsing&parent=browsing&op=list_gene'){
  webpage <- xml2::read_html(url)
  data_html <- rvest::html_nodes(webpage,'#Submit')
  data_html <- gsub('.*value="',"",data_html)
  data_html <- gsub(' .*',"",data_html)
  conditions <- data_html[grepl("-",data_html)]
  genotypes <- unique(conditions)
  ids = list()
  for(i in 1:length(genotypes)){
    l = genotypes[i]
    flycircuit_progress(i, max = length(genotypes), message = l)
    for(n in 0:70){
      n = n*100
      for(s in c("male","female")){
        url = paste0("http://www.flycircuit.tw/modules.php?name=browsing&op=listGene_v2&driverOn=",l,"&genderOn=",s,"&groupON=",n,"&gidON=34#gidName34")
        subpage <- read_html(url)
        subdata <- rvest::html_nodes(subpage,'a')
        subdata <- subdata[grepl("neuron=",subdata)]
        subdata <- gsub('.*neuron=',"",subdata)
        subdata <- gsub("'.*","",subdata)
        subdata <- unlist(subdata[grepl("-",subdata)])
        if(length(subdata)>0){
          ids = c(ids,subdata)
        }
      }
    }
  }
  unique(ids)
}

# hidden
flycircuit_progress <- function (x, max = 100, message = "querying flycircuit") {
  percent <- x / max * 100
  cat(sprintf('\r|%-50s| ~%d%% %s',
              paste(rep('+', percent / 2), collapse = ''),
              floor(percent), message))
  if (x == max)
    cat('\n')
}
