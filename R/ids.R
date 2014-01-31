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
      res=subset(fcidtable,Name%in%x,c(gene_name,Name))
      rownames(res)=res$Name
      as.character(res[x,"gene_name"])
    }
  } else {
    res=subset(fcidtable,idid%in%x,c(gene_name,idid))
    rownames(res)=res$idid
    as.character(res[as.character(x),"gene_name"])
  }
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
      res=subset(fcidtable,Name%in%x,c(idid,Name))
      rownames(res)=res$Name
    } else {
      # assume we have gene_names
      res=subset(fcidtable,gene_name%in%x,c(gene_name,idid))
      rownames(res)=res$gene_name
    }
    res[x,"idid"]
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
      res=subset(fcidtable,gene_name%in%x,c(gene_name,Name))
      rownames(res)=res$gene_name
    }
    res[x,"idid"]
  } else {
    # assume these are idids
    res=subset(fcidtable,idid%in%x,c(Name,idid))
    rownames(res)=as.character(res$idid)
  }
  res[x,"Name"]
}
