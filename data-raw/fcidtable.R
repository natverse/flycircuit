fcidalldf=flycircuit_get_ids(rval = 'data.frame')
load("data-raw/fcidtablev1.0.rda")
fcidtable$v1.0=TRUE
fcidalldf$v1.2=TRUE
fcidtable=merge(fcidtable, fcidalldf, by.x='Name', by.y='neuron', all=T)
fcidtable$v1.0[is.na(fcidtable$v1.0)]=FALSE
fcidtable$v1.2[is.na(fcidtable$v1.2)]=FALSE
