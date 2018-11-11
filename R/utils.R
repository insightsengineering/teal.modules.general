
# from teal.tern
whiteSmallWell <- function(...) {
  shiny::tags$div(class = "well well-sm", style = "background-color: white;", shiny::tags$div(style = "overflow-x: auto;",...))
}

as.global <- function(...) {

  dots <- substitute(list(...))[-1]
  names <- sapply(dots, deparse)

  args <- list(...)

  ge <- globalenv()

  Map(function(x, name) {
    ge[[name]] <- x
  }, args, names)

}

#' @export
empty_string_as_NULL <- function(x) {
  if (identical(x, "")) NULL else x
}

call_fun_dots <- function(fun, str_args) {
  do.call("call", c(list(fun), lapply(str_args, as.name)), quote = TRUE)
}



## from teal.tern
get_rcode_header <- function(title, datanames, datasets, code_data_processing = NULL, packages = NULL) {

  datanames <- c("ASL", setdiff(datanames, "ASL"))

  datanames_import <- if (is.null(code_data_processing)) {
    datanames
  } else {
    datasets$datanames() # because the code_data_processing might have references to all datasets
  }

  data <- lapply(datanames_import, function(x)datasets$get_data(x, reactive = FALSE, filtered=FALSE))
  names(data) <- datanames_import

  comment <- function(txt) {
    paste0("# ", gsub("\n", "\n# ", txt, fixed = TRUE))
  }

  if (!has_source_attribute(data)) {
    "# not all datasets have the 'source' attribute, please contact the app maintainer"
  } else {

    info <- Sys.info()

    txt_location <- if (info['nodename'] == 'rkaub00459.kau.roche.com') {
      sub("/opt/bee_tools/shiny/",  "https://shiny.roche.com/", getwd(), fixed = TRUE)
    } else {
      getwd()
    }

    txt_data <- paste(
      unlist(Map(function(x, name) {
        txt <- paste(name, "<-", attr(x, "source"))
        md5 <- attr(x, "md5sum")
        if (is.null(md5)) txt else paste(txt, "# md5sum:", md5)
      }, data, names(data))),
      collapse = "\n"
    )

    txt_data_processing <- if (is.null(code_data_processing)) {
      ""
    } else {
      paste(c(code_data_processing, ""), collapse = "\n")
    }

    txt_filter <- teal::get_filter_txt(datanames, datasets)


    ## header
    txt_inst_pkgs <- if (is.null(packages)) {
      NULL
    } else {
      vapply(packages, function(pkg) paste0("package: ", pkg, " - ", packageDescription(pkg)$Version), character(1))
    }

    needs_rcd <- any(grepl("radam\\(", txt_data))
    if (needs_rcd) {
      txt_inst_pkgs <- c(
        paste0('devtools::install_github("Rpackages/random.cdisc.data", ref="v',
               packageDescription("random.cdisc.data")$Version,'", host="https://github.roche.com/api/v3")'),
        txt_inst_pkgs
      )
    }


    txt_comment <- paste(c(
      title,
      "",
      paste("Output Created on", format(Sys.time(), "%b %d, %Y"), "by", info['user']),
      paste("with teal app under", txt_location),
      "",
      paste(R.version$version.string, "on", info['nodename']),
      "",
      txt_inst_pkgs
    ), collapse = "\n")


    paste(c(
      comment(txt_comment),
      "",
      vapply(packages, function(pkg)paste0("library(", pkg,")"), character(1)),
      if (needs_rcd) "library(random.cdisc.data)" else NULL,
      "",
      txt_data,
      "",
      txt_data_processing,
      txt_filter,
      "",
      sep = "\n"
    ))
  }



}


has_source_attribute <- function(x) {
  if (is.data.frame(x)) x <- list(x)
  all(vapply(x, function(dat) !is.null(attr(dat, "source")), logical(1)))
}
