# Server =======================================================================
#' Download a CSV File
#'
#' Save and Download a [`data.frame`] (CSV).
#' @param x A reactive [`data.frame`] to be saved.
#' @param name A [`character`] string specifying the name of the file
#'  (without extension and the leading dot).
#' @return
#'  No return value, called for side effects.
#' @family widgets
#' @keywords internal
#' @noRd
export_table <- function(x, name) {
  stopifnot(is.reactive(x))

  downloadHandler(
    filename = function() { make_file_name(name, "csv") },
    content = function(file) {
      x <- x()
      if (!is.data.frame(x) && !is.matrix(x)) x <- as.matrix(x)
      utils::write.csv(
        x = x,
        file = file,
        fileEncoding = "utf-8"
      )
    },
    contentType = "text/csv"
  )
}

#' Download Multiple CSV Files
#'
#' Save and Download several [`data.frame`] (Zip).
#' @param ... Further named arguments ([`data.frame`] to be saved).
#' @param name A [`character`] string specifying the name of the file
#'  (without extension and the leading dot).
#' @return
#'  No return value, called for side effects.
#' @family widgets
#' @keywords internal
#' @noRd
export_multiple <- function(..., name = "archive") {
  tbl <- list(...)
  stopifnot(!is.null(names(tbl)))

  downloadHandler(
    filename = function() { make_file_name(name, "zip") },
    content = function(file) {
      tmpdir <- tempdir()
      on.exit(unlink(tmpdir))

      ## Write CSV files
      fs <- vapply(
        X = names(tbl),
        FUN = function(f) {
          path <- file.path(tmpdir, paste0(f, ".csv"))
          utils::write.csv(
            x = tbl[[f]](),
            file = path,
            row.names = TRUE,
            fileEncoding = "utf-8"
          )
          return(path)
        },
        FUN.VALUE = character(1)
      )

      ## Create Zip file
      utils::zip(zipfile = file, files = fs, flags = "-r9Xj")
    },
    contentType = "application/zip"
  )
}
