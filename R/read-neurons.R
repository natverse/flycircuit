#' Fetch FlyCircuit neuron skeletons from the Taiwan FlyCircuit server
#'
#' @description Reads FlyCircuit skeletons from \url{http://www.flycircuit.tw/},
#'   and bridges them into the FCWB space.
#'
#' @param fc.ids vector of valid FlyCircuit neuron ids. To acquire these in
#'   bulk, see \code{\link{flycircuit_get_ids}} and
#'   \code{\link{flycircuit-ids}}.
#' @param ... additional arguments passed to methods
#' @seealso \code{\link{flycircuit_get_ids}}, \code{\link{flycircuit-ids}},
#'   \code{\link{read.neurons}}
#'
#' @examples
#' \donttest{
#' # Let's read a neuron from the FlyCircuit database
#' library(nat.flybrains)
#' fcn <- flycircuit_read_neurons("Gad1-F-200234")
#' plot3d(fcn)
#' plot3d(FCWB)
#'
#' # We can also read all neurons
#' clear3d()
#' fc.ids = flycircuit_get_ids()
#' fcns <- flycircuit_read_neurons(fc.ids)
#' plot3d(fcns)
#' plot3d(FCWB, alpha = 0.1)
#'
#' # Now mirror all neurons to the right of the brain
#' left.somas <- function(neuron,bound = boundingbox(FCWB.surf)[1,1]+((boundingbox(FCWB.surf)[2,1]-boundingbox(FCWB.surf)[1,1])/2)){
#'   r = nat::rootpoints(neuron)
#'   position = nat::xyzmatrix(neuron$d[r,])
#'   position[,"X"]>bound
#' }
#' leftsomas = unlist(nat::nlapply(fcns,left.somas))
#' fcsleft = nat.templatebrains::mirror_brain(fcns[leftsomas], brain = FCWB)
#' fcns = c(fcns[!names(fcns)%in%names(fcsleft)],fcsleft)
#' }
#' @source \url{http://www.flycircuit.tw/}
#' @seealso \code{\link{flycircuit_get_ids}}, \code{\link{flycircuit_page}}
#' @return A \code{neuronlist} of FlyCircuit neurons registered in the intersex
#'   FCWB brain space
#' @export
flycircuit_read_neurons <- function(fc.ids, ...){
  ids = c()
  fcns = nat::neuronlist()
  for (n in 1:length(fc.ids)){
    baseurl="http://flycircuit.tw/flycircuitSourceData/NeuronData_v1.2/%s/%s_seg001_linesetTransformRelease.swc"
    swc=sprintf(baseurl, fc.ids[n], fc.ids[n])
    ofcn=tryCatch(nat::read.neuron(swc), error = function(e) warning("unable to read neuron: ", fc.ids[n]))
    if(!is.null(ofcn)) {
      fcns = c(fcns, nat::as.neuronlist(ofcn))
      ids = c(ids, fc.ids[n])
    }
  }
  names(fcns) = ids
  fcns=Chiang2FCWB(fcns)
  fcns = nat::nlapply(fcns,reroot_flycircuit_neuron)
  fcns
}

# hidden
reroot_flycircuit_neuron <- function(x){
  x = nat::as.neuron(nat::as.ngraph(x), origin = which(x$d$Label==4))
  x$d$Label = 0
  x$d$Label[x$StartPoint] = 1
  x
}

# hidden
#' @importFrom nat.templatebrains xform_brain
Chiang2FCWB <- function(x, female = grepl("-F-",x)) {
  template_to_use=ifelse(female, "chiangf","chiangm")
  nat::nmapply(xform_brain, x, sample=template_to_use, MoreArgs = list(reference=nat.flybrains::FCWB))
}