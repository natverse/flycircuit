#' Plot FlyCircuit neurons using rgl
#' 
#' @details \itemize{
#'   
#'   \item Colour defaults to black for a single neuron or rainbow(length(id)) 
#'   when more than one neuron is plotted. It can be specified as a function, a 
#'   vector (length(id)) or a single recycled value.
#'   
#'   \item By default when more than 200 neurons are plotted in a single call 
#'   rgl redraw is suspended. This can be >10x faster.
#'   
#'   \item \code{\link[nat]{npop3d}} can be used to remove the last plotted 
#'   neurons.
#'   
#'   \item Plotted soma positions (when \code{soma=TRUE}) is passed as an 
#'   additional argument for plot3d.neuronlist are a prediction based on 
#'   location in the FlyCircuit template brain mapped onto my selected template.
#'   The positions are looked up in the df attribute of the neuronlist.
#'   
#'   }
#' @param id Vector of FlyCircuit gene or neuron names or idids (passed to 
#'   \code{\link{fc_gene_name}})
#' @param col Function or vector specifying colour.
#' @param db Object of class \code{neuronlist} or a character vector naming a 
#'   neuronlist that will be passed to \code{link{get}}.
#' @param ... Additional arguments for \code{plot3d}
#' @return List of rgl stack ids (see rgl::plot3d).
#' @export
#' @seealso \code{\link[rgl]{plot3d}, \link{pop3dfc}, \link{fc_gene_name}}
#' @examples
#' open3d()
#' library(nat)
#' plot3dfc("fru-M-300145", col='green', db=kcs20)
#' npop3d()
#' plot3dfc("fru-M-100014", col='red', db=kcs20, soma=TRUE)
#' rgl.close()
plot3dfc <- function(id, col, db=getOption('nat.default.neuronlist'), ...) {
  if(is.null(db)) stop("Must specify a neuronlist")
  if(is.character(db)) db=get(db)
  
  id=fc_gene_name(id)
  # drop any missing ids with a warning
  missing_ids=setdiff(id, names(db))
  if(nmissing<-length(missing_ids)){
    warning("Dropping ", nmissing,' neurons that are not present in db!')
    id=setdiff(id, missing_ids)
  }
  # bail if we have nothing to plot
  if(!length(id)){
    warning("No valid neurons to plot!")
    return(invisible(NULL))
  }
  
  if(missing(col)) {
    col = if(length(id)>1) rainbow else 'black'
  }
  do.call(plot3d, args = list(x=id, col=col, db=db, ...))
}

#' Remove plotted FlyCircuit neurons (deprecated, see nat::npop3d)
#' 
#' If no neurons are specified, the last plotted are removed.
#' @param x Neurons to remove
#' @param slow Whether to remove neurons one by one (slowly) default: FALSE
#' @param type Type of objects to remove see \code{pop3d}.
#' @export
#' @seealso \code{\link[rgl]{pop3d}}
#' @importFrom rgl pop3d
pop3dfc <- function(x, slow=FALSE, type='shapes') {
  .Deprecated('nat::npop3d', msg = 'Please use npop3d for identical functionality that can work with any neuronlist')
  nat::npop3d(x=x, slow=slow, type=type)
}

#' Plot a 3D surface of the FlyCircuit reference brain
#'
#' @param col The colour of the surface
#' @param alpha The opacity of the surface
#' @param ... Extra arguments to pass to plot3d
#' @importFrom rgl plot3d
#' @export
fcwbsurf <- function(col='grey', alpha=0.3, ...) {
  plot3d(FCWB.surf, col=col, alpha=alpha, ...)
}

#' Plot 3D surfaces of the FlyLight neuropil segmentation in FlyCircuit coordinate space
#'
#' @param ... Extra arguments to pass to plot3d
#' @export
#' @importFrom rgl plot3d
fcwbnpsurf <- function(...) {
  plot3d(FCWBNP.surf, ...)
}

#' Creates a new surface containing specified regions from a given surface
#'
#' @param regionNames A list of regions to include in the surface
#' @param surf A surface which contains a superset of the regions
#' @return A surface containing the specified regions
#' @export
getRegionSurf <- function(regionNames, surf) {
  regionIdxs <- match(regionNames,surf$RegionList)
  if (any(is.na(regionIdxs))) stop("Some regionNames are not present in surf")
  surfNew <- list()
  surfNew$Regions <- surf$Regions[regionNames]
  surfNew$Vertices <- surf$Vertices
  surfNew$RegionList <- surf$RegionList[regionIdxs]
  surfNew$RegionColourList <- surf$RegionColourList[regionIdxs]
  # TODO define a [.surf method
  class(surfNew) <- class(surf)
  surfNew
}

#' Draw surface corresponding to a given list of regions from surface object
#'
#' @param regionNames A list of regions to draw
#' @param surf A surface containing a superset of the regions to draw
#' @param ... Additional arguments passed to plot3d
#' @export
drawRegionSurf <- function(regionNames, surf, ...) {
  surf <- getRegionSurf(regionNames, surf)
  plot3d(surf, ...)
}

#' Return a list of regions contained in a surface
#'
#' @param surf The surface to examine
#' @return Returns a list of the regions in the surface object
#' @export
getRegionsFromSurf <- function(surf) {
  surf$RegionList
}

#' Select regions from a surface
#' 
#' @details if selfun is not set (i.e. it is NULL) then a call will be made to 
#'   select3d() with a message prompting for the user to draw a selection box.
#' @param surf The surface from which regions should be selected
#' @param selfun (optional) function returning determining whether x,y,z coords 
#'   are within a selection region.
#' @export
#' @seealso \code{\link{select3d}}
selectRegionsFromSurf <- function(surf, selfun=NULL) {
  if(is.null(selfun)){
    message("Draw a selection box in the RGL window")
    selfun <- select3d()
  }
  selverts <- selfun(surf$Vertices[, 1:3])
  selpointnums <- surf$Vertices[selverts,'PointNo']
  surf$RegionList[lapply(surf$Regions, function(x) any(selpointnums %in% unlist(x))) == TRUE]
}


#' Scan through a set of flycircuit neurons, plotting each with plot3dfc
#' 
#' Can also choose to select specific neurons along the way and navigate 
#' forwards and backwards.
#' 
#' @param neurons character vector of names of neuron to plot.
#' @param col the color with which to plot the neurons (default \code{'red'}).
#' @param Verbose logical indicating that info about each selected neuron should
#'   be printed (default \code{TRUE}).
#' @param Wait logical indicating that there should be a pause between each 
#'   displayed neuron.
#' @param sleep time to pause between each displayed neuron when 
#'   \code{Wait=TRUE}.
#' @param extrafun an optional function called when each neuron is plotted, with
#'   args \code{gene_name} and \code{selected}.
#' @param selected_file an optional path to a \code{yaml} file that already 
#'   contains a selection.
#' @param selected_col the color in which selected neurons (such as those 
#'   specified in \code{selected_file}) should be plotted.
#' @param yaml a logical indicating that selections should be saved to disk in 
#'   \code{yaml} rather than \code{rda} format.
#' @param ... extra arguments to pass to \code{\link{plot3dfc}}.
#'   
#' @return A character vector of names of any selected neurons, \code{NULL} if 
#'   none selected.
#' @importFrom yaml yaml.load_file
#' @importFrom yaml as.yaml
#' @export
dpscan <- function(neurons, db=getOption('nat.default.neuronlist'), col='red', 
                   Verbose=T, Wait=T, sleep=0.1, extrafun=NULL, 
                   selected_file=NULL, selected_col='green', yaml=TRUE, ...) {
  neurons=fc_gene_name(neurons)
  
  frames <- length(neurons)
  if(length(col)==1) col <- rep(col,frames)
  selected <- character()
  i <- 1
  if(!is.null(selected_file) && file.exists(selected_file)) {
    selected <- yaml.load_file(selected_file)
    if(!all(names(selected) %in% neurons)) stop("Mismatch between selection file and neurons.")
  }
  
  savetodisk <- function(selected, selected_file) {
    if(is.null(selected_file)) selected_file <- file.choose(new=TRUE)
    if(yaml){
      if(!grepl("\\.yaml$",selected_file)) selected_file <- paste(selected_file,sep="",".yaml")
      message("Saving selection to disk as ", selected_file, ".")
      writeLines(as.yaml(selected), con=selected_file)
    } else {
      if(!grepl("\\.rda$", selected_file)) selected_file <- paste(selected_file, sep="", ".rda")
      save(selected, file=selected_file)
      message("Saving selection to disk as ", selected_file)
    }
    selected_file
  }
  
  while(TRUE){
    if(i > length(neurons) || i < 1) break
    n <- neurons[i]
    cat("Current neuron:", n, "(", i, "/", length(neurons), ")\n")
    pl <- plot3dfc(n, col=ifelse(n %in% selected, selected_col, col[i]), db=db, ...)
    # call user supplied function
    more_rgl_ids <- list()
    if(!is.null(extrafun))
      more_rgl_ids <- extrafun(n, selected=selected)
    if(Wait){
      chc <- readline("Return to continue, b to go back, s to select, d [save to disk], t to stop, c to cancel (without returning a selection): ")
      if(chc=="c" || chc=='t'){
        sapply(pl, rgl.pop, type='shape')
        sapply(more_rgl_ids, rgl.pop, type='shape')
        break
      }
      if(chc=="s") {
        if(n %in% selected) {
          message("Deselected: ", n)
          selected <- setdiff(selected, n)
        } else selected <- union(selected, n)
      }
      if(chc=="b") i <- i-1
      else if (chc=='d') savetodisk(selected, selected_file)
      else i <- i+1
    } else {
      Sys.sleep(sleep)
      i <- i+1
    }
    sapply(pl, rgl.pop, type='shape')
    sapply(more_rgl_ids, rgl.pop, type='shape')
  }
  if(chc=='c') return(NULL)
  if(!is.null(selected_file)) savetodisk(selected, selected_file)
  selected
}

