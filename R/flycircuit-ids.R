#' Scrape FlyCircuit.tw for neuron IDs
#'
#' @description Scrape neuron IDs from \url{http://www.flycircuit.tw/}, which
#'   may then be read into R using \code{\link{fc_read_neurons}}. Note,
#'   this function can be quite slow to run.
#'
#' @param url webpage of FlyCircuit neuron IDs to scrape
#' @param rval Whether to return the neuron IDs alone or a data.frame with
#'   additional information.
#'
#' @details There have now been multiple releases of FlyCircuit neurons. This
#'   function has been tested with v1.2 current as of 2019-08-25. The original
#'   id functions (\code{\link{flycircuit-ids}}) are targeted at version 1.0.
#'   Several neuron ids have been retired between v1.0 and v1.2. Furthermore the
#'   integer ids used in v1.0 (\code{idid}) are not compatible with those in
#'   v1.2 (\code{nid}).
#'
#' @examples
#' \dontrun{
#' # Let's get all the FlyCircuit neurons we can
#' fc.ids = fc_get_ids()
#' fcns <- fc_read_neurons(fc.ids)
#' plot3d(fcns)
#' plot3d(FCWB, alpha = 0.1)
#' }
#' @source \url{http://www.flycircuit.tw/}
#' @seealso \code{\link{fc_read_neurons}},
#'   \code{\link{fc_page}}, \code{\link{flycircuit-ids}}
#' @return A vector of all FlyCircuit IDs
#' @export
fc_get_ids <-  function(url=paste0("http://www.flycircuit.tw/modules.php",
                                           "?name=browsing&op=listGene_v2"),
                                rval=c('neuronid','data.frame')) {
  if(!requireNamespace("xml2", quietly = TRUE))
    stop("Please install suggested package xml2 to use fc_get_ids()!")
  rval=match.arg(rval)
  df <- fc_image_summary(url)
  pb <-
    progress::progress_bar$new(total = sum(df$n, na.rm = T),
                               format = "  :current/:total ids [:bar]  eta: :eta",
                               show_after = 2)
  idinfo=list()
  for (i in seq_len(nrow(df))) {
    urlparts=httr::parse_url(df$link[i])
    baseurlparts=httr::parse_url(url)
    urlparts[c("scheme", "hostname", "port")]=baseurlparts[c("scheme", "hostname", "port")]
    n = 0
    nids = c()
    neurons = c()
    nfound = 100 # signalling value
    # they are shown in batches of 100
    while (isTRUE(nfound == 100)) {
      urlparts$query$groupON=n
      u=httr::build_url(urlparts)
      subdata <- xml2::read_html(u) %>%
        rvest::html_nodes('#menu3 a')
      nfound = length(subdata)
      if (isTRUE(nfound > 0)) {
        neurons = c(neurons, xml2::xml_text(subdata))
        nidtxt=stringr::str_match(xml2::xml_attr(subdata,'href'), 'nid=([0-9]{1,3})')[,2]
        nids = c(nids, as.integer(nidtxt))
        pb$tick(nfound)
        n = n + 100
      }
    }
    if(length(neurons))
      idinfo[[i]]=dplyr::tibble(nid=nids, neuron=neurons)
    # message("Driver: ", df$driver[i], " sex:", df$sex[i], " nfound: ", nfound)
  }
  iddf=dplyr::bind_rows(idinfo, .id = 'row')
  if(rval=='data.frame') {
    df$row=seq_len(nrow(df))
    m=merge(iddf, df[c("driver","sex","row")], by='row', all.x=T, sort=F)
    m[['row']]=NULL
    m
  } else {
    iddf$neuron
  }
}

# Summary of numbers of images for each driver/sex
fc_image_summary <- function(url) {
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
