#' Scrape FlyCircuit.tw for neuron IDs 
#'
#' @description Scrape neuron IDs from \url{http://www.flycircuit.tw/}, which may then
#' be read into R using \code{\link{flycircuit_read_neurons}}. Note, this function 
#' can be quite slow to run.
#'
#' @param url webpage of FlyCircuit neuron IDs to scrape
#' 
#' @examples 
#' \dontrun{
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
flycircuit_get_ids <-  function(url=paste0("http://www.flycircuit.tw/modules.php",
                                           "?name=browsing&op=listGene_v2")){
  if(!requireNamespace("xml2", quietly = TRUE))
    stop("Please install suggested package xml2 to use flycircuit_get_ids()!")
  
  df <- flycircuit_image_summary(url)
  pb <-
    progress::progress_bar$new(total = sum(df$n, na.rm = T),
                               format = "  :current/:total ids [:bar]  eta: :eta",
                               show_after = 2)
  ids = c()
  for (i in seq_len(nrow(df))) {
    urlparts=httr::parse_url(df$link[i])
    baseurlparts=httr::parse_url(url)
    urlparts[c("scheme", "hostname", "port")]=baseurlparts[c("scheme", "hostname", "port")]
    n = 0
    nfound = 100 # signalling value
    # they are shown in batches of 100
    while (isTRUE(nfound == 100)) {
      urlparts$query$groupON=n
      u=httr::build_url(urlparts)
      subpage <- xml2::read_html(u)
      subdata <- rvest::html_nodes(subpage, 'a')
      subdata <- subdata[grepl("neuron=", subdata)]
      subdata <- gsub('.*neuron=', "", subdata)
      subdata <- gsub("'.*", "", subdata)
      subdata <- unlist(subdata[grepl("-", subdata)])
      nfound = length(subdata)
      if (isTRUE(nfound > 0)) {
        ids = c(ids, subdata)
        pb$tick(nfound)
        n = n + 100
      }
    }
    # message("Driver: ", df$driver[i], " sex:", df$sex[i], " nfound: ", nfound)
  }
  unique(ids)
}

# Summary of numbers of images for each driver/sex
flycircuit_image_summary <- function(url) {
  webpage <- xml2::read_html(url)
  data_nodes <- rvest::html_nodes(webpage,'#Submit')
  values=xml2::xml_attr(data_nodes, 'value')
  data_nodes=data_nodes[grepl("images", values)]
  values=xml2::xml_attr(data_nodes, 'value')
  links=xml2::xml_attr(data_nodes, 'onclick')
  
  res=stringr::str_match(values, '(.*?)\\s+\\(([0-9]+)\\s+images\\)')
  df=as.data.frame(res[,-1], stringsAsFactors=FALSE)
  names(df)=c("driver", 'n')
  df$n=as.integer(df$n)
  df$sex=NA_character_
  df$sex[grepl('genderOn=female', links)]='female'
  df$sex[grepl('genderOn=male', links)]='male'
  df$link=stringr::str_match(links, "'(.*)'")[,2]
  df
}
