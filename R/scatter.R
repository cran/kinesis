# UI ===========================================================================
#' Scatter Plot UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [scatter_server()]
#' @family plot modules
#' @keywords internal
#' @export
scatter_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Scatter Plot"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Variables"),
        ## Input: select axes
        selectize_ui(ns("axis1"), label = tr_("Component X")),
        selectize_ui(ns("axis2"), label = tr_("Component Y")),
        ## Input: aesthetics mapping
        selectize_ui(ns("extra_quali"), label = tr_("Extra qualitative variable")),
        selectize_ui(ns("extra_quanti"), label = tr_("Extra quantitative variable")),
        ## Input: add ellipses
        radioButtons(
          inputId = ns("wrap"),
          label = tr_("Wrap:"),
          choiceNames = c(tr_("None"), tr_("Tolerance ellipse"),
                          tr_("Confidence ellipse"), tr_("Convex hull")),
          choiceValues = c("none", "tol", "conf", "hull"),
        ),
        checkboxGroupInput(
          inputId = ns("level"),
          label = tr_("Ellipse level:"),
          selected = "0.95",
          choiceNames = c("68%", "95%", "99%"),
          choiceValues = c("0.68", "0.95", "0.99")
        ),
        checkboxInput(inputId = ns("grid"), label = tr_("Grid"), value = TRUE)
      ), # sidebar
      helpText(
        tr_("Click and drag to select an area, then double-click to zoom in."),
        tr_("Double-click again to reset the zoom.")
      ),
      layout_columns(
        col_widths = c(8, 4),
        output_plot(
          id = ns("plot"),
          tools = graphics_ui(ns("par"), col_quant = FALSE, lty = FALSE, asp = TRUE),
          title = tr_("Scatter Plot"),
          click = ns("plot_click"),
          dblclick = ns("plot_dblclick"),
          brush = brushOpts(
            id = ns("plot_brush"),
            resetOnNew = TRUE
          ),
          height = "100%"
        ),
        card(
          helpText(tr_("Click the plot to select rows of data.")),
          gt::gt_output(outputId = ns("info"))
        )
      )
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Scatter Plot Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`data.frame`].
#' @return
#'  No return value, called for side effects.
#' @seealso [scatter_ui()]
#' @family plot modules
#' @keywords internal
#' @export
scatter_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI -----
    quanti <- subset_quantitative(x)
    quali <- subset_qualitative(x)

    axis1 <- update_selectize_colnames("axis1", x = quanti)
    axis2 <- update_selectize_colnames("axis2", x = quanti, exclude = axis1)
    col_quali <- update_selectize_colnames("extra_quali", x = quali)
    col_quanti <- update_selectize_colnames("extra_quanti", x = quanti)

    ## Extra variables -----
    extra_quali <- select_data(quali, col_quali, drop = TRUE)
    extra_quanti <- select_data(quanti, col_quanti, drop = TRUE)

    ## Interactive zoom -----
    ## When a double-click happens, check if there's a brush on the plot.
    ## If so, zoom to the brush bounds; if not, reset the zoom.
    range_plot <- reactiveValues(x = NULL, y = NULL)
    observe({
      range_plot$x <- brush_xlim(input$plot_brush)
      range_plot$y <- brush_ylim(input$plot_brush)
    }) |>
      bindEvent(input$plot_dblclick)
    info <- reactive({
      ## With base graphics, need to tell it what the x and y variables are.
      nearPoints(x(), input$plot_click, xvar = axis1(), yvar = axis2(),
                 threshold = 5)
    })

    ## Ellipses -----
    wrap <- reactive({
      level <- as.numeric(input$level)
      switch(
        input$wrap,
        tol = function(x, y, z, ...) dimensio::viz_tolerance(x, y, group = z, level = level, ...),
        conf = function(x, y, z, ...) dimensio::viz_confidence(x, y, group = z, level = level, ...),
        hull = function(x, y, z, ...) dimensio::viz_hull(x, y, group = z, ...),
        function(...) invisible()
      )
    })

    ## Graphical parameters -----
    param <- graphics_server("par")

    ## Build plot -----
    plot_scatter <- reactive({
      ## Select data
      req(x(), axis1(), axis2())

      coord_x <- x()[[axis1()]]
      coord_y <- x()[[axis2()]]

      col <- param$col_quali(extra_quali())
      pch <- param$pch(extra_quali())
      cex <- param$cex(extra_quanti())

      ## Build plot
      function() {
        graphics::plot(
          x = coord_x,
          y = coord_y,
          type = "p",
          xlim = range_plot$x,
          ylim = range_plot$y,
          xlab = axis1(),
          ylab = axis2(),
          panel.first = if (isTRUE(input$grid)) graphics::grid() else NULL,
          col = col,
          pch = pch,
          cex = cex,
          asp = param$asp,
          las = 1
        )

        if (isTruthy(extra_quali())) {
          ## Add ellipses
          wrap()(x = coord_x, y = coord_y, z = extra_quali(), color = param$pal_quali)

          ## Add legend
          labels <- unique(extra_quali())
          keep <- !is.na(labels)
          cols <- unique(col)
          symb <- unique(pch)
          graphics::legend(
            x = "topleft",
            legend = labels[keep],
            col = if (length(cols) == 1) cols else cols[keep],
            pch = if (length(symb) == 1) symb else symb[keep]
          )
        }
      }
    })

    ## Render table -----
    output$info <- gt::render_gt({
      gt::gt(info(), rownames_to_stub = TRUE) |>
        gt::tab_options(table.width = "100%")
    })

    ## Render plot -----
    render_plot("plot", x = plot_scatter)
  })
}
