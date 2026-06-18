# UI =====================================================================
render_export_button <- function(id) {
  uiOutput(outputId = NS(id, "download_button"))
}

# Server =======================================================================
#' Download a CSV File
#'
#' Save and Download a [`data.frame`] (CSV).
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`data.frame`] to be saved.
#' @param name A [`character`] string specifying the name of the file
#'  (without extension and the leading dot).
#' @param label A [`character`] string giving the label that should appear on
#'  the button.
#' @return
#'  No return value, called for side effects.
#' @keywords internal
export_table <- function(id, x, name, label = tr_("Download results")) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    output$download_button <- renderUI({
      req(x())
      downloadButton(
        outputId = session$ns("download"),
        label = label
      )
    })

    output$download <- downloadHandler(
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
  })
}

#' Download Multiple CSV Files
#'
#' Save and Download several [`data.frame`] (Zip).
#' @param ... Further named arguments ([`data.frame`] to be saved).
#' @inheritParams export_table
#' @return
#'  No return value, called for side effects.
#' @keywords internal
export_multiple <- function(id, ..., name = "archive", label = tr_("Download results")) {
  tbl <- list(...)
  stopifnot(!is.null(names(tbl)))

  moduleServer(id, function(input, output, session) {
    output$download_button <- renderUI({
      req(tbl[[1L]]())
      downloadButton(
        outputId = session$ns("download"),
        label = label
      )
    })

    output$download <- downloadHandler(
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
        utils::zip(zipfile = file, files = fs, flags = "-r9Xjq")
      },
      contentType = "application/zip"
    )
  })
}
