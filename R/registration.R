# Internal function to add registrations to chiang template brains
add_registrations <- function() {
  # Note that the Morpho affine registrations need to be inverted
  # see e.g. elmr::tpsreg for discussion of source/reference conventions
  
  regdir <- system.file('reg', package='flycircuit')
  # message("regdir = ", shQuote(regdir))
  fcwb_chiangm.init <-
    matrix(
      c(
        1.60264349915336,
        -0.000358837442878542,
        -3.92349041969571e-05,
        0,
        0.00100048096382857,
        -1.60948892721639,
        -0.00120710755931993,
        0,
        -0.00111465000314184,
        -0.00318808344028277,
        -1.63401121094495,
        0,
        -410.548210347152,
        135.688686868135,
        92.1573824715654,
        1
      ),
      ncol = 4
    )
  fcwb_chiangm <-
    reglist(
      fcwb_chiangm.init,
      file.path(regdir, "FCWB_typicalbrainmale_01_warp_m0g80c8e1e-1x26r4.list")
    )
  fcwb_chiangf.init <-
    matrix(
      c(
        1.67470990047486,
        -0.00220660815755018,
        0.000385243316701086,
        0,
        -0.00107324451603582,
        -1.67165686360037,
        0.00404368361703313,
        0,
        -0.00238812801210672,
        -0.00986058851616913,
        -1.69193418619646,
        0,
        -449.39343128491,
        145.684269516489,
        103.550463130095,
        1
      ),
      ncol = 4
    )
  fcwb_chiangf <-
    reglist(
      fcwb_chiangf.init,
      file.path(regdir, "FCWB_typicalbrainfemale_01_warp_m0g80c8e1e-1x26r4.list/")
    )
  
  nat.templatebrains::add_reglist(fcwb_chiangf, reference = nat.flybrains::FCWB, sample = 'chiangf')
  nat.templatebrains::add_reglist(fcwb_chiangm, reference = nat.flybrains::FCWB, sample = 'chiangm')
  
}
