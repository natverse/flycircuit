library(testthat)
library(flycircuit)

op=options(flycircuit.scoremat='kcs20scores')
test_package("flycircuit")
options(op)
