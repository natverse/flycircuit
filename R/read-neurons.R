#' Fetch FlyCircuit neuron skeletons from the Taiwan FlyCircuit server
#'
#' @description Reads FlyCircuit skeletons from \url{http://www.flycircuit.tw/},
#'   and bridges them into the FCWB space.
#'
#' @param fc.ids vector of valid FlyCircuit neuron ids. To acquire these in
#'   bulk, see \code{\link{fc_get_ids}} and
#'   \code{\link{flycircuit-ids}}.
#' @param xform Whether or not to tranform neurons from their original space to
#'   the \code{\link{FCWB}} template space.
#' @param ... additional arguments passed to methods
#' @seealso \code{\link{fc_get_ids}}, \code{\link{flycircuit-ids}},
#'   \code{\link{read.neurons}}
#'
#' @examples
#' \donttest{
#' # Let's read a neuron from the FlyCircuit database
#' library(nat.flybrains)
#' fcn <- fc_read_neurons("Gad1-F-200234")
#' plot3d(fcn)
#' plot3d(FCWB)
#' }
#'
#' \dontrun{
#' # We can also read all neurons
#' clear3d()
#' # nb this will take tens of minutes to hours
#' fc.ids = fc_get_ids()
#' fcns <- fc_read_neurons(fc.ids)
#' plot3d(fcns)
#' plot3d(FCWB, alpha = 0.1)
#'
#' ## Now mirror all neurons to the right of the brain
#' # estimate whether soma is on left or right of midline
#' # nb this assumes that FCWB brain surface is mirror symmetric
#' # which is apporoximately but not exactly the case
#' left.somas <- function(neuron, surf = FCWB.surf) {
#'   bb=boundingbox(surf)
#'   midline=(bb[1,1]+bb[2,1])/2
#'   r = nat::rootpoints(neuron)
#'   somaposition = nat::xyzmatrix(neuron$d[r,])
#'   somaposition[,"X"]>midline
#' }
#' leftsomas = unlist(nat::nlapply(fcns,left.somas))
#' fcsleft = nat.templatebrains::mirror_brain(fcns[leftsomas], brain = FCWB)
#' fcns = c(fcns[!names(fcns)%in%names(fcsleft)],fcsleft)
#' }
#' @source \url{http://www.flycircuit.tw/}
#' @seealso \code{\link{fc_get_ids}}, \code{\link{fc_page}}
#' @return A \code{neuronlist} of FlyCircuit neurons registered in the intersex
#'   FCWB brain space
#' @export
fc_read_neurons <- function(fc.ids, xform=TRUE, ...){
  fcns = nat::neuronlist()
  for (n in 1:length(fc.ids)){
    baseurl="http://flycircuit.tw/flycircuitSourceData/NeuronData_v1.2/%s/%s_seg001_linesetTransformRelease.swc"
    swc=sprintf(baseurl, fc.ids[n], fc.ids[n])
    ofcn=tryCatch(nat::read.neuron(swc), error = function(e){
      warning("unable to read neuron: ", fc.ids[n])
      NULL
    })
    if(!is.null(ofcn)) {
      ofcn$id = fc.ids[n]
      fcns = nat::union(fcns, nat::as.neuronlist(ofcn))
    }
  }
  fcns.good = fcns[sapply(fcns, nat::is.neuron)]
  names(fcns.good) = sapply(fcns.good, function(x) x$id)
  fcns.good[,"id"] =  names(fcns.good)
  fcns.good[,"dataset"] = "flycircuit"
  fcns.good = nat::nlapply(fcns.good, fc_reroot_neuron)
  if(isTRUE(xform))
    fcns.good=Chiang2FCWB(fcns.good, OmitFailures = TRUE)
  fcns.good
}

# hidden
fc_reroot_neuron <- function(x){
  x = nat::as.neuron(nat::as.ngraph(x), origin = which(x$d$Label==4))
  x$d$Label = 0
  x$d$Label[x$StartPoint] = 1
  x
}

# hidden
#' @importFrom nat.templatebrains xform_brain
Chiang2FCWB <- function(x, female = grepl("-F-", names(x)), ...) {
  
  if(any(female)) {
    xf=xform_brain(x, subset=female, sample='chiangf', reference=nat.flybrains::FCWB, ...)
  }
  if(!all(female)) {
    xfm=xform_brain(xf, subset=!female, sample='chiangm', reference=nat.flybrains::FCWB, ...)
  }
  xfm
}
