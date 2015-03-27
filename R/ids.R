#' Convert between different FlyCircuit identifiers
#' 
#' @description \code{fc_gene_name} returns a \code{character} vector of 
#'   \code{gene_name}s of fly circuit neurons given \code{idid} or neuron 
#'   \code{Name}
#' @details Depends on fcidtable dataframe
#' @param x Vector of flycircuit identifiers (integer idid, chaacter/factor 
#'   gene_name/Name)
#' @return character vector of gene_name/Name or integer vector of idid.
#' @export
#' @rdname flycircuit-ids
#' @family flycircuit-ids
#' @examples
#' fc_gene_name(1)
#' # "FruMARCM-M002262_seg001"
fc_gene_name<-function(x){
  if(is.factor(x)) x=as.character(x)
  if(is.character(x)){
    # FIXME check that ALL x values look like gene_names
    if(regexpr('seg',x[1])>=0) return(x)
    else {
      res=match(x,flycircuit::fcidtable$Name)
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
fc_idid<-function(x){
  if(is.factor(x)) x=as.character(x)
  if(is.character(x)){
    # check if we have been given neuron Names
    xAreNames=grepl("^[\\w\\-]+-[MFX]+-\\d+$",x,perl=TRUE)
    if(any(xAreNames)){
      # but make sure they all are ...
      if(!all(xAreNames)) 
        stop("ambiguous flycircuit neuron Names:",paste(x[!xAreNames],collapse=", "))
      res=match(x,flycircuit::fcidtable$Name)
    } else {
      # assume we have gene_names
      res=match(x,flycircuit::fcidtable$gene_name)
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
fc_neuron<-function(x){
  if(is.factor(x)) x=as.character(x)
  if(is.character(x)){
    # check if we have been given neuron Names
    xAreNames=grepl('^\\w+-[MFX]+-\\d+$',x,perl=TRUE)
    if(any(xAreNames)){
      # but make sure they all are ...
      if(!all(xAreNames)) 
        stop("ambiguous flycircuit neuron Names:",paste(x[!xAreNames],collapse=", "))
      return(x)
    } else {
      # assume we have gene_names
      res=match(x,flycircuit::fcidtable$gene_name)
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
fc_sex<-function(x){
  nids=fc_neuron(x)
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
