## code to prepare `data` for testing examples
library(scda)
rADAE <- synthetic_cdisc_data("latest")$adae # nolint object_name_linter
usethis::use_data(rADAE)

rADLB <- synthetic_cdisc_data("latest")$adlb # nolint object_name_linter
usethis::use_data(rADLB)

rADRS <- synthetic_cdisc_data("latest")$adrs # nolint object_name_linter
usethis::use_data(rADRS)

rADSL <- synthetic_cdisc_data("latest")$adsl # nolint object_name_linter
usethis::use_data(rADSL)

rADTTE <- synthetic_cdisc_data("latest")$adtte # nolint object_name_linter
usethis::use_data(rADTTE)
