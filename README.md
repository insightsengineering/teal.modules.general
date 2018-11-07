
# teal.general drafts of very general teal moduls

The modules in this package are generic modules that should work with any data set (not necessarily for clinical trials data). We are prototyping these modules and they are not meant for wide adoption at the moment.

For general reusable modules see:

* teal base package modules
* teal.tern modules
* teal.osprey 
* teal.goshawk



```r
install.packages(c("data.table", "cli"))

devtools::install_github(
  repo = "Rpackages/random.cdisc.data",
  ref = "v0.1.0", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github("Roche/rtables",
  upgrade_dependencies = FALSE, build_vignettes = FALSE)

devtools::install_github(
  repo = "Rpackages/tern",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "Rpackages/teal",
  ref = "v0.0.4", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "Rpackages/teal.tern",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "Rpackages/gClinBiomarker",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

```



