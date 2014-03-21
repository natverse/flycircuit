library(testthat)

op=options(flycircuit.scoremat='kcs20scores')
test_check("flycircuit")
options(op)
