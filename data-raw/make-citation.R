library(RefManageR)
bib=ReadBib('data-raw/flycircuit.bib')

con=file('inst/CITATION', open = 'w')
writeLines(c(
  'citHeader(
  "If you are using this package, you will most likely need to cite ",
  "two papers, Chiang et al. (2011) for the raw image data and Costa et al. (2016) ",
  "for the functionality contained within this R package as well as its ",
  "associated cell type annotations."
)'
), con = con)
dput(bib, file = con)
close(con)
stop()

bib=bibtex::read.bib('data-raw/flycircuit.bib')

bib[[1]]$key
# bib[[1]]$key=NULL
# bib[[2]]$key=NULL

bib$mheader=c("If you are using this package, you will most likely need to cite ",
             "two papers, Chiang et al. (2011) for the raw image data and Costa et al. (2016) ",
             "for the functionality contained within this R package as well as its ",
             "associated cell type annotations.")

# dput(bib, 'inst/CITATION')

writeLines(format(bib, style = "R"), 'inst/CITATION')
