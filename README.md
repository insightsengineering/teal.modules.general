
# teal.general drafts of very general teal moduls

The modules in this package are generic modules that should work with any data set (not necessarily for clinical trials data). We are prototyping these modules and they are not meant for wide adoption at the moment.

For general reusable modules see:

* teal base package modules
* teal.modules.clinical modules
* teal.osprey 
* teal.goshawk


For this visit [https://github.roche.com/settings/tokens](https://github.roche.com/settings/tokens) there Generate New Token with name "read" that has all repo options activated. Copy the token into `~/.github_token` on bee.


```r
install.packages(c("data.table", "ggmosaic"))

devtools::install_github(
  repo = "Rpackages/random.cdisc.data",
  ref = "new-simple", 
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
  ref = "v0.0.5", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "Rpackages/teal.modules.clinical",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE
)

devtools::install_github(
  repo = "Rpackages/teal.general",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE, build_vignettes = FALSE,
  auth_token = readLines("~/.github_token")
)

```



