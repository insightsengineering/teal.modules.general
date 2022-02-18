# teal.modules.general

This package contains a set of standard `teal` modules for you to include in your `teal` applications. 
These include modules for:

- Viewing data (including `tm_variable_browser`, `tm_data_table` and `tm_file_viewer`)
- Visualizing data (`tm_g_scatterplot`, `tm_g_association` and many more)
- Understanding missing and outlier values within your data (`tm_missing_data` and `tm_outliers`)
- Performing simple data analysis (`tm_a_pca` and `tm_a_regression`)

These modules work with both `CDISC` and general relational data.  

# Installation

This repository requires a personal access token to install see here [creating and using PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token). Once this is set up, to install the latest released version of the package run:

```
Sys.setenv(GITHUB_PAT = "your_access_token_here")
install.packages("devtools")
devtools::install_github("insightsengineering/teal.modules.general@*release")
```

<< Some comment about handling dependencies here >>

# Quickstart

```
library(teal.modules.general)



```

# Further Information

See package vignettes `browseVignettes(package = "teal.modules.general)` for further examples.





## Clone and install manually:

1. Clone the repository

   The repository can be downloaded directly from the `github.com` site as an archive (see [Github tutorial on cloning to learn more](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/cloning-a-repository-from-github/cloning-a-repository)). Alternatively, Git command-line tools offer the same functionality, without the need for manually downloading and unpacking the archive, but require authentication to Github. You can authenticate using an SSH key pair or a Personal Access Token (PAT). Please refer to excellent Github tutorials on [connecting to Github using SSH](https://docs.github.com/en/github/authenticating-to-github) or [creating and using PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token).
   * Using PAT. Input in the Git Bash console, PowerShell or any Linux shell:

      ```
      $ git clone https://github.com/insightsengineering/teal.modules.general.git
      Username: your_username_goes_here
      Password: your_token_goes_here
      ```
   * Using SSH. If set up properly, the repository is ready to be cloned executing:

      ```
      $ git clone https://github.com/insightsengineering/teal.modules.general.git
      ```
   This creates a subdirectory `teal.modules.general` containing the cloned repository.

2. Build and install

   The native R tools provide a quick way to install a package. Run in PowerShell or any Linux shell:
   ```
   $ R CMD build teal.modules.general
   ```
   This command builds the package and creates an archive. The name of the archive is output by the command at the end of building the package. Then input in the shell:
   ```
   $ Rscript -e 'install.packages("name_of_the_archive")
   ```
   Here is an example of a real command (with name_of_the_archive substituted by the output of the build command):
   ```
   $ Rscript -e 'install.packages("teal.modules.general_0.2.10.9000.tar.gz")'
   ```
