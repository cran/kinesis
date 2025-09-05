# UI ===========================================================================
#' Principal Components Analysis UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @param center A [`logical`] scalar: should the variables be shifted to be
#'  zero centered?
#' @param scale A [`logical`] scalar: should the variables be scaled to unit
#'  variance?
#' @param help A [`character`] string giving a short help text.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [pca_server()]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
pca_ui <- function(id, center = TRUE, scale = TRUE, help = NULL) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("PCA"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Principal Components Analysis"),
        helpText(textOutput(ns("help"))),
        checkboxInput(
          inputId = ns("center"),
          label = tr_("Center"),
          value = center
        ),
        checkboxInput(
          inputId = ns("scale"),
          label = tr_("Scale"),
          value = scale
        ),
        selectize_ui(
          id = ns("sup_row"),
          label = tr_("Supplementary individuals"),
          multiple = TRUE
        ),
        selectize_ui(
          id = ns("sup_col"),
          label = tr_("Supplementary quantitative variables"),
          multiple = TRUE
        ),
        selectize_ui(
          id = ns("sup_quali"),
          label = tr_("Supplementary qualitative variables"),
          multiple = TRUE
        ),
        bslib::input_task_button(id = ns("go"), label = tr_("(Re)Compute")),
        downloadButton(
          outputId = ns("download"),
          label = tr_("Download results")
        )
      ), # sidebar
      multivariate_ui(ns("pca")),
      border_radius = FALSE,
      fillable = TRUE
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Principal Components Analysis Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame`.
#' @return A reactive [`dimensio::PCA-class`] object.
#' @seealso [pca_ui()]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
pca_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI -----
    quanti <- subset_quantitative(x)
    quali <- subset_qualitative(x)

    sup_row <- update_selectize_rownames("sup_row", x = x)
    sup_col <- update_selectize_colnames("sup_col", x = quanti)
    sup_quali <- update_selectize_colnames("sup_quali", x = quali, select = TRUE)

    ## Check data -----
    old <- reactive({ x() }) |> bindEvent(input$go)
    notify_change(session$ns("change"), x, old, title = tr_("PCA"))
    output$help <- renderText({
      if (inherits(x(), "LogRatio")) {
        txt <- tr_("PCA is computed on centered log-ratio (CLR), you should check the data transformation first.")
        return(txt)
      }
    })

    ## Compute PCA -----
    compute_pca <- ExtendedTask$new(
      function(x, center, scale, rank, sup_row, sup_col, sup_quali) {
        mirai::mirai({
          param <- list(object = x, center = center, scale = scale, rank = rank,
                        sup_row = arkhe::seek_rows(x, names = sup_row),
                        sup_col = sup_col)
          if (is.data.frame(x)) {
            param$sup_quali <- sup_quali
          }
          do.call(dimensio::pca, param)
        }, environment())
      }
    ) |>
      bslib::bind_task_button("go")

    observe({
      compute_pca$invoke(x = x(), center = input$center, scale = input$scale,
                         rank = input$rank, sup_row = sup_row(),
                         sup_col = sup_col(), sup_quali = sup_quali())
    }) |>
      bindEvent(input$go)

    results <- reactive({
      notify(compute_pca$result(), title = tr_("Principal Components Analysis"))
    })

    multivariate_server("pca", x = results, y = x)

    ## Export -----
    output$download <- downloadHandler(
      filename = function() { make_file_name("pca", "zip") },
      content = function(file) {
        dimensio::export(results(), file = file, flags = "-r9Xj")
      },
      contentType = "application/zip"
    )

    results
  })
}
