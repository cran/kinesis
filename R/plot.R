# UI ===========================================================================
#' Plot UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @param tools A (list of) input elements.
#' @param title A [`character`] string giving the card title.
#' @param note A [`character`] string giving a note to be placed in the footer.
#' @param ... Further parameters to be passed to [shiny::plotOutput()].
#' @return A [htmltools::div()] tag.
#' @family widgets
#' @keywords internal
output_plot <- function(id, ..., tools = NULL, title = NULL, note = NULL) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  gear <- popover(
    icon("gear"),
    title = tr_("Tools"),
    placement = "auto",
    tools,
    actionButton(
      inputId = ns("download"),
      label = tr_("Download"),
      icon = icon("download")
    )
  )

  footer <- if (!is.null(note)) card_footer(note) else NULL

  card(
    id = ns("card"),
    full_screen = TRUE,
    card_header(
      title, gear,
      class = "d-flex justify-content-between"
    ),
    card_body(
      plotOutput(outputId = ns("plot"), ...)
    ),
    footer
  )
}

brush_xlim <- function(e) {
  if (is.null(e)) return(NULL)
  c(e$xmin, e$xmax)
}

brush_ylim <- function(e) {
  if (is.null(e)) return(NULL)
  c(e$ymin, e$ymax)
}

# Server =======================================================================
# https://stackoverflow.com/a/46961131
#' Plot Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`function`] recording the plot.
#' @param ... Further parameters to be passed to [shiny::renderPlot()].
#' @return
#'  No return value, called for side effects.
#' @family widgets
#' @keywords internal
render_plot <- function(id, x, ...) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Show modal dialog
    observe({ showModal(download_plot(session$ns)) }) |>
      bindEvent(input$download)

    ## Plot
    output$plot <- renderPlot(x()(), ...)

    ## Preview
    output$preview <- renderImage({
      req(x())

      ## Write to a temporary PNG file
      outfile <- tempfile(fileext = ".png")

      grDevices::png(
        filename = outfile,
        width = input$width,
        height = input$height,
        units = "in",
        res = 72
      )
      x()()
      grDevices::dev.off()

      ## Return a list containing information about the image
      list(
        src = outfile,
        contentType = "image/png",
        style = "height:300px; width:auto; max-width:100%;"
      )
    }, deleteFile = TRUE)

    ## Download
    output[["pdf"]] <- export_plot(input, x, format = "pdf")
    output[["png"]] <- export_plot(input, x, format = "png")
  })
}

#' Export Plot Modal
#'
#' @param ns A [namespace][shiny::NS()] function.
#' @keywords internal
#' @noRd
download_plot <- function(ns) {
  modalDialog(
    title = tr_("Save plot - Preview"),
    size = "l",
    easyClose = FALSE,
    fade = FALSE,
    div(
      plotOutput(outputId = ns("preview")),
      style = "text-align: center;"
    ),
    layout_column_wrap(
      width = 1/3,
      textInput(
        inputId = ns("name"),
        label = tr_("File name"),
        value = "plot"
      ),
      numericInput(
        inputId = ns("width"),
        label = tr_("Width (in)"),
        min = 0.5,
        value = 7
      ),
      numericInput(
        inputId = ns("height"),
        label = tr_("Height (in)"),
        min = 0.5,
        value = 7
      )
    ),
    footer = tagList(
      modalButton(tr_("Cancel")),
      downloadButton(
        outputId = ns("pdf"),
        label = "PDF",
        icon = icon("download")
      ),
      downloadButton(
        outputId = ns("png"),
        label = "PNG",
        icon = icon("download")
      )
    )
  )
}

#' Download Plot
#'
#' Save and Download a graphic.
#' @param input Inputs selected by the user.
#' @param x A reactive [`function`] recording the plot.
#' @param format A [`character`] string specifying the file extension.
#' @return
#'  No return value, called for side effects.
#' @keywords internal
#' @noRd
export_plot <- function(input, x, format) {
  downloadHandler(
    filename = function() { make_file_name(input$name, format) },
    content = function(file) {
      device <- switch (
        format,
        pdf = function(x, ...) grDevices::pdf(x, ...),
        png = function(x, ...) grDevices::png(x, ..., units = "in", res = 300),
        stop(tr_("Unknown graphics device."), call. = FALSE)
      )

      device(file, width = input$width, height = input$height)
      x()()
      grDevices::dev.off()
    }
  )
}
