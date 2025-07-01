# HELP TEXT

#' Make File Name
#'
#' @param name A [`character`] string specifying the name of the file
#'  (without extension and the leading dot).
#' @param ext A [`character`] string specifying the file extension.
#' @param project A [`character`] string specifying the name of the project.
#' @param timestamp A [`character`] string specifying the timestamp
#'  (defaults to current date and time).
#' @return A [`character`] string.
#' @family widgets
#' @keywords internal
#' @noRd
make_file_name <- function(name, ext, project = NULL, timestamp = NULL) {
  project <- if (is.null(project)) "" else paste0(project, "_")
  timestamp <- timestamp %||% format(Sys.time(), "%y%m%d_%H%M%S")

  sprintf("%s%s_%s.%s", project, name, timestamp, ext)
}

#' Build an URL
#'
#' @param package A [`character`] string giving the name of a package.
#' @return A [`character`] string (URL).
#' @keywords internal
#' @noRd
url_tesselle <- function(package = NULL) {
  if (is.null(package)) return("https://www.tesselle.org/")
  sprintf("https://packages.tesselle.org/%s/", package)
}

#' Citing \R Packages
#'
#' @param x A [`character`] vector giving the name of one or more package.
#' @return Citations properly formated in HTML.
#' @keywords internal
#' @noRd
cite_package <- function(x = NULL) {
  x <- c("kinesis", x)
  lapply(
    X = x,
    FUN = function(x) {
      bib <- format(utils::citation(x)[[1]], style = "text")
      txt <- paste0(vapply(X = bib, FUN = markdown, FUN.VALUE = character(1)))
      HTML(txt)
    }
  )
}

#' Citing a Publication
#'
#' @param author A [`character`] string giving the name of the author(s).
#' @param author An [`integer`] or a [`character`] string giving the publication
#'  year.
#' @param doi A [`character`] string giving the DOI. If not `NULL`, it will be
#'  used to create a link.
#' @param text A [`logical`] scalar. If `FALSE`, the citation will be printed
#'  in parentheses.
#' @param before,after A [`character`] string to be inserted before and after
#'  the citation, resp.
#' @param html A [`logical`] scalar. If `TRUE` (the default), the text is marked
#'  as HTML.
#' @return An author-date citation in HTML.
#' @keywords internal
#' @noRd
cite_article <- function(author, year, doi = NULL, text = TRUE,
                         before = "", after = "", html = TRUE) {
  right <- paste0(")", after)
  if (is.null(doi)) {
    link <- tags$span(year, .noWS = "outside")
  } else {
    link <- url_doi(doi, label = year)
  }

  if (text) {
    cite <- tags$span(before, author, "(", link, right)
  } else {
    cite <- tags$span(paste0("(", author, ", "), link, right,
                      .noWS = c("after-begin", "before-end"))
  }
  if (!html) cite <- as.character(cite)
  cite
}

info_article <- function(...) {
  cite_article(..., before = icon("info-circle"), after = ".")
}

url_doi <- function(x, label = NULL, prefix = FALSE) {
  if (is.null(label)) label <- x
  url <- sprintf("https://doi.org/%s", x)
  no_ws <- if (prefix) c("after") else c("before", "after")
  link <- tags$a(label, href = url, target = "_blank", role="doc-biblioref", .noWS = no_ws)
  if (!prefix) return(link)
  list("DOI:", link, ".")
}

help_warranty <- function(...) {
  tags$p(
    tr_("This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY.")
  )
}

help_data <- function(...) {
  withTags(
    p(
      tr_("You can import your data in CSV format."),
      tr_("It assumes that you keep your data tidy:"),
      tr_("each variable must be saved in its own column and each sample must be saved in its own row.")
    )
  )
}

help_cite <- function(package) {
  list(
    tags$p(
      tr_("If you use this application in your research, you must report and cite it properly to ensure transparency of your results."),
      tr_("Moreover, authors and maintainers of this project are more likely to continue their work if they see that it's being used and valued by the research community.")
    ),
    tags$p(tr_("To cite in your publications, please use:")),
    cite_package(package)
  )
}

help_license <- function(...) {
  withTags(
    p(
      tr_("This app is distributed as a free and open source R package:"),
      a("packages.tesselle.org/kinesis", href = url_tesselle("kinesis"),
        target = "_blank", rel = "external", .noWS = "after"), "."
    )
  )
}

help_tesselle <- function(...) {
  withTags(
    list(
      p(HTML(
        tr_("This app is a part of the <strong>tesselle</strong> project, a collection of packages for research and teaching in archaeology."),
        tr_("The <strong>tesselle</strong> packages focus on quantitative analysis methods developed for archaeology."),
        tr_("They can be used to explore and analyze common data types in archaeology.")
      )),
      p(
        tr_("For more information and relevant links, see"),
        a("tesselle.org", href = url_tesselle(), target = "_blank",
          rel = "external", .noWS = "after"), "."
      )
    )
  )
}
