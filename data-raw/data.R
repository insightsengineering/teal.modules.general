## code to prepare `data` for testing examples
library(scda)
rADAE <- synthetic_cdisc_data("latest")$adae # nolint
usethis::use_data(rADAE)

rADLB <- synthetic_cdisc_data("latest")$adlb # nolint
usethis::use_data(rADLB)

rADRS <- synthetic_cdisc_data("latest")$adrs # nolint
usethis::use_data(rADRS)

rADSL <- synthetic_cdisc_data("latest")$adsl # nolint
usethis::use_data(rADSL)

rADTTE <- synthetic_cdisc_data("latest")$adtte # nolint
usethis::use_data(rADTTE)
