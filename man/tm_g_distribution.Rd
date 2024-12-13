% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_g_distribution.R
\name{tm_g_distribution}
\alias{tm_g_distribution}
\title{\code{teal} module: Distribution analysis}
\usage{
tm_g_distribution(
  label = "Distribution Module",
  dist_var,
  strata_var = NULL,
  group_var = NULL,
  freq = FALSE,
  ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
  ggplot2_args = teal.widgets::ggplot2_args(),
  bins = c(30L, 1L, 100L),
  plot_height = c(600, 200, 2000),
  plot_width = NULL,
  pre_output = NULL,
  post_output = NULL,
  decorators = NULL
)
}
\arguments{
\item{label}{(\code{character(1)}) Label shown in the navigation item for the module or module group.
For \code{modules()} defaults to \code{"root"}. See \code{Details}.}

\item{dist_var}{(\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
Variable(s) for which the distribution will be analyzed.}

\item{strata_var}{(\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
Categorical variable used to split the distribution analysis.}

\item{group_var}{(\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
Variable used for faceting plot into multiple panels.}

\item{freq}{(\code{logical}) optional, whether to display frequency (\code{TRUE}) or density (\code{FALSE}).
Defaults to density (\code{FALSE}).}

\item{ggtheme}{(\code{character}) optional, \code{ggplot2} theme to be used by default. Defaults to \code{"gray"}.}

\item{ggplot2_args}{(\code{ggplot2_args}) optional, object created by \code{\link[teal.widgets:ggplot2_args]{teal.widgets::ggplot2_args()}} with settings for all the plots or named list of \code{ggplot2_args} objects for plot-specific settings. The argument is merged with options variable \code{teal.ggplot2_args} and default module setup.

List names should match the following: \code{c("default", "Histogram", "QQplot")}.

For more details see the vignette: \code{vignette("custom-ggplot2-arguments", package = "teal.widgets")}.}

\item{bins}{(\code{integer(1)} or \code{integer(3)}) optional,  specifies the number of bins for the histogram.
\itemize{
\item When the length of \code{bins} is one: The histogram bins will have a fixed size based on the \code{bins} provided.
\item When the length of \code{bins} is three: The histogram bins are dynamically adjusted based on vector of \code{value}, \code{min},
and \code{max}.
Defaults to \code{c(30L, 1L, 100L)}.
}}

\item{plot_height}{(\code{numeric}) optional, specifies the plot height as a three-element vector of
\code{value}, \code{min}, and \code{max} intended for use with a slider UI element.}

\item{plot_width}{(\code{numeric}) optional, specifies the plot width as a three-element vector of
\code{value}, \code{min}, and \code{max} for a slider encoding the plot width.}

\item{pre_output}{(\code{shiny.tag}, optional)\cr
with text placed before the output to put the output into context. For example a title.}

\item{post_output}{(\code{shiny.tag}, optional) with text placed after the output to put the output
into context. For example the \code{\link[shiny:helpText]{shiny::helpText()}} elements are useful.}

\item{decorators}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}  (\code{list} of \code{teal_transform_module}, named \code{list} of \code{teal_transform_module} or \code{NULL}) optional, if not \code{NULL}, decorator for tables or plots included in the module. When a named list of \code{teal_transform_module}, the decorators are applied to the respective output objects.

Otherwise, the decorators are applied to all objects, which is equivalent as using the name \code{default}.

See section "Decorating \code{tm_g_distribution}" below for more details.}
}
\value{
Object of class \code{teal_module} to be used in \code{teal} applications.
}
\description{
Module is designed to explore the distribution of a single variable within a given dataset.
It offers several tools, such as histograms, Q-Q plots, and various statistical tests to
visually and statistically analyze the variable's distribution.
}
\section{Decorating \code{tm_g_distribution}}{


This module generates the following objects, which can be modified in place using decorators::
\itemize{
\item \code{histogram_plot} (\code{ggplot2})
\item \code{qq_plot} (\code{data.frame})
\item \code{summary_table} (\code{data.frame})
\item \code{test_table} (\code{data.frame})
}

Decorators can be applied to all outputs or only to specific objects using a
named list of \code{teal_transform_module} objects.
The \code{"default"} name is reserved for decorators that are applied to all outputs.
See code snippet below:

\if{html}{\out{<div class="sourceCode">}}\preformatted{tm_g_distribution(
   ..., # arguments for module
   decorators = list(
     default = list(teal_transform_module(...)), # applied to all outputs
     histogram_plot = list(teal_transform_module(...)), # applied only to `histogram_plot` output
     qq_plot = list(teal_transform_module(...)) # applied only to `qq_plot` output
     summary_table = list(teal_transform_module(...)) # applied only to `summary_table` output
     test_table = list(teal_transform_module(...)) # applied only to `test_table` output
   )
)
}\if{html}{\out{</div>}}

For additional details and examples of decorators, refer to the vignette
\code{vignette("decorate-modules-output", package = "teal")} or the \code{\link[teal:teal_transform_module]{teal::teal_transform_module()}} documentation.
}

\examples{
\dontshow{if (require("ggpmisc", quietly = TRUE) && require("ggpp", quietly = TRUE) && require("goftest", quietly = TRUE) && require("MASS", quietly = TRUE) && require("broom", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# general data example
data <- teal_data()
data <- within(data, {
  iris <- iris
})

app <- init(
  data = data,
  modules = list(
    tm_g_distribution(
      dist_var = data_extract_spec(
        dataname = "iris",
        select = select_spec(variable_choices("iris"), "Petal.Length")
      )
    )
  )
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}
\dontshow{\}) # examplesIf}
\dontshow{if (require("ggpmisc", quietly = TRUE) && require("ggpp", quietly = TRUE) && require("goftest", quietly = TRUE) && require("MASS", quietly = TRUE) && require("broom", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# CDISC data example
data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

vars1 <- choices_selected(
  variable_choices(data[["ADSL"]], c("ARM", "COUNTRY", "SEX")),
  selected = NULL
)

app <- init(
  data = data,
  modules = modules(
    tm_g_distribution(
      dist_var = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
          selected = "BMRKR1",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      strata_var = data_extract_spec(
        dataname = "ADSL",
        filter = filter_spec(
          vars = vars1,
          multiple = TRUE
        )
      ),
      group_var = data_extract_spec(
        dataname = "ADSL",
        filter = filter_spec(
          vars = vars1,
          multiple = TRUE
        )
      )
    )
  )
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}
\dontshow{\}) # examplesIf}
}
\section{Examples in Shinylive}{
\describe{
  \item{example-1}{
    \href{https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAoksZwAjoNrv2CsBISqDC0ogT+uLpetHCk1Ky6ALz2Ts62AGTpuu7Rvv6B6BFR3rHxSSkuGVk53nkBRCbkoqRF0aUJyQ6VupnZnrVwfmAAsgCCAMrjrSVxHRVpPdX9PoP+TEREMNMxs+VdaUoAxLpSEGrUuvxQpFC6cAAesKgiSlc3BsZc1AD6b1A2r2utyMugA7rRSAALFTsP6REBKXS6HyhD7IxihJQAX0UECUaFQaJUEL8ECRf3KcMRuj4QhEonKdGapKRSNIMG+El+oVIGPogk01mprMuPO+WhYlKB3wevPUpG+olQcAILJFrL+0Hg5X8KNEEWF6tEcBEGnKxtNCqVKvYEoxUHoIm+BEhRFoBDEQz1-jkkX8AAVYtwMAAZCgSKE+w1I3Ei2O6XG42gmXTsFTkZiWHQ2WwIsm6UTQiCsUbodgEgAk3kileNjB0jFxWKUYCxAF0gA}{Open in Shinylive}
    \if{html}{\out{<iframe class="iframe_shinylive" src="https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAoksZwAjoNrv2CsBISqDC0ogT+uLpetHCk1Ky6ALz2Ts62AGTpuu7Rvv6B6BFR3rHxSSkuGVk53nkBRCbkoqRF0aUJyQ6VupnZnrVwfmAAsgCCAMrjrSVxHRVpPdX9PoP+TEREMNMxs+VdaUoAxLpSEGrUuvxQpFC6cAAesKgiSlc3BsZc1AD6b1A2r2utyMugA7rRSAALFTsP6REBKXS6HyhD7IxihJQAX0UECUaFQaJUEL8ECRf3KcMRuj4QhEonKdGapKRSNIMG+El+oVIGPogk01mprMuPO+WhYlKB3wevPUpG+olQcAILJFrL+0Hg5X8KNEEWF6tEcBEGnKxtNCqVKvYEoxUHoIm+BEhRFoBDEQz1-jkkX8AAVYtwMAAZCgSKE+w1I3Ei2O6XG42gmXTsFTkZiWHQ2WwIsm6UTQiCsUbodgEgAk3kileNjB0jFxWKUYCxAF0gA" style="height: 800px; width: 100vw; max-width: 1400px; border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem; position: absolute; left: 50\%; margin-top: 30px; transform: translateX(-50\%); z-index: 1"></iframe>}}
    \if{html}{\out{<a style='height: 800px; display: block;'></a>}}
  }
  \item{example-2}{
    \href{https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAoksZwAjoNrv2CsBISqDC0ogT+uLpetHCk1Ky6ALz2Ts62AGTpuu7Rvv6B6BFR3rHxSSkuGVk53nkBRCbkoqRF0aUJyQ6VupnZnrVwfmAAsgCCAMrjrSVxHRVpPdX9PoP+TEREMNMxs+VdaUoAxLoAwgAiAJLjJ7r8UKRQunAAHrCoIkp3DwbGXNQA+l8oDZPvdHkZdAB3WikAAWKnYQMiICUul0ozO4wAMj9dH8MEDEIhGBjsUoAL6KCAAKyIKn+AGs4KxRIiwbYIfw4CYoMJSP8CPxQgR-rT6UyWcBoPBWUC5ABdJRKLQsUQARlxBFhdIIYn+ojgIg0cH4fggaJVjFoUHoIgF2toutlYOAwH8pKx-nl8siBCGoywwyK-hOAHlHAA5BwATWDYHGzgAGv45HJcKjdAajeR+OUI44sZ6IFSlGhULiVDCzWigeUkRm+EIRKJyo3hGJq2i8TB-hJAaFSFb6IJNNYM13bgP-pa62D-i9B+p+aJUHA-eOJ7cwdK9Ml3ZjPXgNxOs2vSOVTxp9av1+bNxOtTqxOVLdbbXB7U-nQ9XfuyWBvV9f0AHFXDwXR-AAIWGLAAGksDVFM02PTdLxzcooJg+DEKPO97zRGA+Vod5d10AAxUYsQTdM8PwkxaGeE1ygoqjXFoicqXvZD2MzRcHmnFhZ34hdmCvFc107e8gR3DCwA9CIULRejqHIRhymU1TrwkxSu0tFtkj0tUaPwrtCJU4iRD2VIdM4zduPvCRGCIQRUAEtTkiBednkXMSb0kzdpNgUi-0PYy6NoFSZHUiLNPE28TItVUX1VIydIIoiSKslwbJQ2y0VsqkqVoExdHYFRVKXbRBlTXQUTvUR4QgVhRnQdgywAEm8SIOoNRgdEYKlySUMByXlIA}{Open in Shinylive}
    \if{html}{\out{<iframe class="iframe_shinylive" src="https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAoksZwAjoNrv2CsBISqDC0ogT+uLpetHCk1Ky6ALz2Ts62AGTpuu7Rvv6B6BFR3rHxSSkuGVk53nkBRCbkoqRF0aUJyQ6VupnZnrVwfmAAsgCCAMrjrSVxHRVpPdX9PoP+TEREMNMxs+VdaUoAxLoAwgAiAJLjJ7r8UKRQunAAHrCoIkp3DwbGXNQA+l8oDZPvdHkZdAB3WikAAWKnYQMiICUul0ozO4wAMj9dH8MEDEIhGBjsUoAL6KCAAKyIKn+AGs4KxRIiwbYIfw4CYoMJSP8CPxQgR-rT6UyWcBoPBWUC5ABdJRKLQsUQARlxBFhdIIYn+ojgIg0cH4fggaJVjFoUHoIgF2toutlYOAwH8pKx-nl8siBCGoywwyK-hOAHlHAA5BwATWDYHGzgAGv45HJcKjdAajeR+OUI44sZ6IFSlGhULiVDCzWigeUkRm+EIRKJyo3hGJq2i8TB-hJAaFSFb6IJNNYM13bgP-pa62D-i9B+p+aJUHA-eOJ7cwdK9Ml3ZjPXgNxOs2vSOVTxp9av1+bNxOtTqxOVLdbbXB7U-nQ9XfuyWBvV9f0AHFXDwXR-AAIWGLAAGksDVFM02PTdLxzcooJg+DEKPO97zRGA+Vod5d10AAxUYsQTdM8PwkxaGeE1ygoqjXFoicqXvZD2MzRcHmnFhZ34hdmCvFc107e8gR3DCwA9CIULRejqHIRhymU1TrwkxSu0tFtkj0tUaPwrtCJU4iRD2VIdM4zduPvCRGCIQRUAEtTkiBednkXMSb0kzdpNgUi-0PYy6NoFSZHUiLNPE28TItVUX1VIydIIoiSKslwbJQ2y0VsqkqVoExdHYFRVKXbRBlTXQUTvUR4QgVhRnQdgywAEm8SIOoNRgdEYKlySUMByXlIA" style="height: 800px; width: 100vw; max-width: 1400px; border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem; position: absolute; left: 50\%; margin-top: 30px; transform: translateX(-50\%); z-index: 1"></iframe>}}
    \if{html}{\out{<a style='height: 800px; display: block;'></a>}}
  }
}
}

