## code to prepare `data` for testing examples
library(scda)
rADAE <- synthetic_cdisc_data("latest")$adae
usethis::use_data(rADAE)

rADLB <- synthetic_cdisc_data("latest")$adlb
usethis::use_data(rADLB)

rADRS <- synthetic_cdisc_data("latest")$adrs
usethis::use_data(rADRS)

rADSL <- synthetic_cdisc_data("latest")$adsl
usethis::use_data(rADSL)

rADTTE <- synthetic_cdisc_data("latest")$adtte
usethis::use_data(rADTTE)
