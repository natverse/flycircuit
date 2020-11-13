#' Convert between different FlyCircuit identifiers
#' 
#' @description \code{fc_gene_name} returns a \code{character} vector of 
#'   \code{gene_name}s of fly circuit neurons given \code{idid} or neuron 
#'   \code{Name}
#' @details Depends on fcidtable dataframe
#' @param x Vector of flycircuit identifiers (integer idid, chaacter/factor 
#'   gene_name/Name)
#' @param ignore.case whether to ignore the case of the input.
#' @return character vector of gene_name/Name or integer vector of idid.
#' @export
#' @name flycircuit-ids
#' @family flycircuit-ids
#' @examples
#' fc_gene_name(1)
#' # "FruMARCM-M002262_seg001"
fc_gene_name<-function(x, ignore.case=TRUE){
  if(is.factor(x)) x=as.character(x)
  if(is.character(x)){
    # FIXME check that ALL x values look like gene_names
    if(regexpr('seg',x[1])>=0) {
      if(ignore.case) {
        return(flycircuit::fcidtable$gene_name[match(tolower(x), tolower(flycircuit::fcidtable$gene_name))])
      } else {
        return(x)
      }
    } else {
      if(ignore.case) {
        res=match(tolower(x), tolower(flycircuit::fcidtable$Name))
      } else {
        res=match(x,flycircuit::fcidtable$Name)
      }
    }
  } else {
    res=match(x,flycircuit::fcidtable$idid)
  }
  flycircuit::fcidtable$gene_name[res]
}

#' @description \code{fc_idid} returns an integer \code{idid} of a fly circuit
#'   neuron given \code{gene_name} or neuron \code{Name}
#' @export
#' @rdname flycircuit-ids
#' @examples
#' isTRUE(fc_idid("FruMARCM-M002262_seg001")==1)
fc_idid<-function(x, ignore.case=TRUE){
  if(is.factor(x)) x=as.character(x)
  if(is.character(x)){
    # check if we have been given neuron Names
    xAreNames=grepl("^[\\w\\-]+-[MFXmfx]+-\\d+$",x,perl=TRUE)
    if(any(xAreNames)){
      # but make sure they all are ...
      if(!all(xAreNames)) 
        stop("ambiguous flycircuit neuron Names:",paste(x[!xAreNames],collapse=", "))
      res=match(x,flycircuit::fcidtable$Name)
    } else {
      # assume we have gene_names
      if(ignore.case) {
        res=match(tolower(x), tolower(flycircuit::fcidtable$gene_name))
      } else {
        res=match(x,flycircuit::fcidtable$gene_name)
      }
    }
    flycircuit::fcidtable$idid[res]
  } else {
    x
  }
}

#' @description \code{fc_neuron} gets the neuron Name of a fly circuit neuron
#'   given gene_name or idid
#' @export
#' @rdname flycircuit-ids
#' @examples
#' isTRUE(fc_neuron("FruMARCM-M002262_seg001")=="fru-M-200266")
fc_neuron<-function(x, ignore.case=TRUE){
  if(is.factor(x)) x=as.character(x)
  if(is.character(x)){
    # check if we have been given neuron Names
    xAreNames=grepl('^\\w+-[MFXmfx]+-\\d+$',x,perl=TRUE)
    if(any(xAreNames)){
      # but make sure they all are ...
      if(!all(xAreNames)) 
        stop("ambiguous flycircuit neuron Names:",paste(x[!xAreNames],collapse=", "))
      if(ignore.case) {
        return(flycircuit::fcidtable$Name[match(tolower(x), tolower(flycircuit::fcidtable$Name))])
      } else {
        return(x)
      }
    } else {
      # assume we have gene_names
      res=match(x,flycircuit::fcidtable$gene_name)
      if(ignore.case) {
        res=match(tolower(x), tolower(flycircuit::fcidtable$gene_name))
      } else {
        res=match(x,flycircuit::fcidtable$gene_name)
      }
    }
  } else {
    # assume these are idids
    res=match(x,flycircuit::fcidtable$idid)
  }
  flycircuit::fcidtable$Name[res]
}

#' return sex of the animal for one or more FlyCircuit identifiers
#' 
#' The sex is computed by normalising the identifier using fc_neuron and then
#' using the middle letter (e.g. -M-) to identify the sex.
#' 
#' @inheritParams fc_neuron
#' @return a character vector with values "M", "F" or \code{NA_character_}
#' @export
#' @examples
#' fc_sex(1)
#' fc_sex(c("FruMARCM-M001589_seg001","TPHMARCM-596M_seg1"))
fc_sex<-function(x, ignore.case=TRUE){
  nids=fc_neuron(x, ignore.case=ignore.case)
  sexes=rep(NA_character_,length(nids))
  sexes[grepl("-M-",nids,fixed=T)]="M"
  sexes[grepl("-F-",nids,fixed=T)]="F"
  sexes
}
  
#' Return the flycircuit gene_name identifier for a file
#'
#' This works for a variety of tested filenames including things like:
#'   FCWB_..._01_warp...
#' @param file Path to file(s) to extract gene_name
#' @param checkExists Check that calculated gene_name exists in identifiers table
#' @return Character vector of gene_names (or NA if checkExists fails)
#' @export
#' @seealso \code{\link{fc_gene_name}, \link{fcidtable}}
#' @examples
#' fcgn_forfile('FruMARCM-M002373_seg002_03.nrrd-files/FruMARCM-M002373_seg002_03.nrrd.am')
fcgn_forfile<-function(file,checkExists=FALSE){
  # get rid of any leading directories
  gn=basename(file)
  #trim off template brain
  gn=sub("^((IS2|FC[^_]+|JFRC[^_]+)_)+","",gn)
  # trim off extension
  gn=sub("\\.[^.]+",'',gn)
  # trim off channel and other suffix
  gn=sub("(seg[^_]*)_.*$","\\1",gn)
  if(checkExists){
    missing=!gn%in%flycircuit::fcidtable$gene_name
    if(any(missing)){
      warning("gene_name:",gn[missing]," for file:",file[missing]," not present in fcid table")
      gn[missing]=NA_character_
    }
  }
  gn
}

#' Antennal lobe glomerulus (where applicable) for FlyCircuit identifiers
#' 
#' These comes from manual annotations carried out for Costa et al.
#' 
#' @param x A neuron identifier. When missing defaults to all neurons annotated 
#'   with an ALGlomerulus tag.
#' @return a character vector of glomerulus names, named by the neuron
#'   identifier
#' @export
#' @seealso \code{\link{fc_neuron_type}}, \code{\link{fc_gene_name}}
#' @examples
#' fc_glom('FruMARCM-F000446_seg001')
#' # how many neurons annotated with any glomerulus
#' length(fc_glom())
#' # how many neurons annotated for each glomerulus
#' table(fc_glom())
fc_glom<-function(x=NULL){
  gloms=flycircuit::annotation[flycircuit::annotation$annotation_class=='ALGlomerulus', ]
  if(!is.null(x)){
    idids=fc_idid(x)
    gloms=gloms[match(idids,gloms$neuron_idid),]
  }
  structure(gloms[,"text"], .Names=fc_gene_name(gloms$neuron_idid))
}

#' Return metadata including NeuronType for FlyCircuit neurons
#' 
#' @description \code{fc_neuron_type} depends on a more generic worker function 
#'   \code{fc_annotated_class}, which may be used to construct other convenience
#'   functions.
#'   
#' @param x A neuron identifier. When missing defaults to all neurons annotated 
#'   with the given annotation class.
#' @param regex An optional \code{\link[base]{regex}} used to filter the 
#'   annotation text values of the matching neurons.
#' @param ... Additional arguments passed to \code{\link[base]{grepl}}.
#' @return a character vector of annotation values, named by the neuron 
#'   identifier
#' @details In our schema, annotations are key-value pairs in which an 
#'   annotation class, such as \bold{NeuronType}, may have multiple values (e.g.
#'   \emph{gamma Kenyon cell}).
#' @seealso \code{\link[base]{grepl}}, \code{\link{fc_gene_name}}, 
#' @export
#' @examples 
#' # how many neurons are annotated with any type?
#' length(fc_neuron_type())
#' # how many types of neuron
#' length(unique(fc_neuron_type()))
#' # how many neurons of each type are present
#' table(fc_neuron_type())
#' # find all the Kenyon cells
#' table(fc_neuron_type(regex="Kenyon"))
#' table(fc_neuron_type(regex="gamma Kenyon"))
#' table(fc_neuron_type(regex="gamma.*Kenyon"))
fc_neuron_type<-function(x=NULL, regex=NULL, ...){
  fc_annotated_class("NeuronType", x, regex = regex, ...)
}

#' @export
#' @rdname fc_neuron_type
#' @param class The annotation class to select (for code{fc_annotated_class}).
fc_annotated_class<-function(class, x=NULL, regex=NULL, ...){
  types=flycircuit::annotation[flycircuit::annotation$annotation_class==class, ]
  if(!is.null(x)){
    idids=fc_idid(x)
    types=types[match(idids,types$neuron_idid),]
  }
  res=structure(types[,"text"], .Names=fc_gene_name(types$neuron_idid))
  if(!is.null(regex)) {
    res[grepl(regex, res, ...)]
  } else res
}
