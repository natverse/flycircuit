#' Get the gene_name of a fly circuit neuron given idid or neuron Name
#'
#' Depends on neuron dataframe
#' @param x Vector of idids or neuron Names
#' @return character vector of gene_names
#' @export
#' @seealso \code{\link{fc_idid,fc_neuron}}
#' @examples
#' fc_gene_name(1)
#' # "FruMARCM-M002262_seg001"
fc_gene_name<-function(x){
  if(is.factor(x)) x=as.character(x)
  if(is.character(x)){
    # FIXME check that ALL x values look like gene_names
    if(regexpr('seg',x[1])>=0) return(x)
    else {
      load_fcdb("neuron")
      res=subset(neuron,Name%in%x,c(gene_name,Name))
      rownames(res)=res$Name
      as.character(res[x,"gene_name"])
    }
  } else {
    load_fcdb("neuron")
    res=subset(neuron,idid%in%x,c(gene_name,idid))
    rownames(res)=res$idid
    as.character(res[as.character(x),"gene_name"])
  }
}

#' Get the idid of a fly circuit neuron given gene_name or neuron Name
#'
#' Depends on neuron dataframe
#' @param x Vector of gene_names or neuron Names
#' @return integer vector of idid
#' @export
#' @seealso \code{\link{fc_gene_name,fc_neuron}}
#' @examples
#' isTRUE(fc_idid("FruMARCM-M002262_seg001")==1)
fc_idid<-function(x){
  if(is.factor(x)) x=as.character(x)
  if(is.character(x)){
    load_fcdb("neuron")
    # check if we have been given neuron Names
    xAreNames=grepl("^[\\w\\-]+-[MFX]+-\\d+$",x,perl=TRUE)
    if(any(xAreNames)){
      # but make sure they all are ...
      if(!all(xAreNames)) 
        stop("ambiguous flycircuit neuron Names:",paste(x[!xAreNames],collapse=", "))
      res=subset(neuron,Name%in%x,c(idid,Name))
      rownames(res)=res$Name
    } else {
      # assume we have gene_names
      res=subset(neuron,gene_name%in%x,c(gene_name,idid))
      rownames(res)=res$gene_name
    }
    res[x,"idid"]
  } else {
    x
  }
}

#' Get the neuron Name of a fly circuit neuron given gene_name or idid
#'
#' Depends on neuron dataframe
#' @param x Vector of gene_names or idids
#' @return character vector of Names
#' @export
#' @seealso \code{\link{fc_gene_name,fc_idid}}
#' @examples
#' isTRUE(fc_neuron("FruMARCM-M002262_seg001")=="fru-M-200266")
fc_neuron<-function(x){
  if(is.factor(x)) x=as.character(x)
  load_fcdb("neuron")
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
      res=subset(neuron,gene_name%in%x,c(gene_name,Name))
      rownames(res)=res$gene_name
    }
    res[x,"idid"]
  } else {
    # assume these are idids
    res=subset(neuron,idid%in%x,c(Name,idid))
    rownames(res)=as.character(res$idid)
  }
  res[x,"Name"]
}
