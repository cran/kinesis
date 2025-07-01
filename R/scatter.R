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
        selectize_ui(id = ns("axis1"), label = tr_("Component X")),
        selectize_ui(id = ns("axis2"), label = tr_("Component Y")),
        ## Input: aesthetics mapping
        selectize_ui(id = ns("extra_quali"), label = tr_("Extra qualitative variable")),
        selectize_ui(id = ns("extra_quanti"), label = tr_("Extra quantitative variable")),
        ## Input: linear regression
        checkboxInput(
          inputId = ns("regression"),
          label = tr_("Linear regression"),
          value = FALSE
        ),
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
    ## Select columns -----
    quanti <- reactive({
      req(x())
      i <- which(arkhe::detect(x = x(), f = is.numeric, margin = 2))
      colnames(x())[i]
    })
    quali <- reactive({
      req(x())
      i <- which(arkhe::detect(x = x(), f = is.numeric, margin = 2, negate = TRUE))
      colnames(x())[i]
    })
    axis1 <- update_selectize_values("axis1", x = quanti)
    axis2 <- update_selectize_values("axis2", x = quanti, exclude = axis1)
    extra_quali <- update_selectize_values("extra_quali", x = quali)
    extra_quanti <- update_selectize_values("extra_quanti", x = quanti)

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

    ## Linear regression -----
    model <- reactive({
      req(x(), axis2(), axis1())
      if (!isTRUE(input$regression)) return(NULL)

      n <- nrow(x())
      group <- if (isTruthy(extra_quali())) x()[[extra_quali()]] else rep("", n)
      by(
        data = x(),
        INDICES = group,
        FUN = function(x) {
          vars <- stats::as.formula(sprintf("%s~%s", axis2(), axis1()))
          fit <- stats::lm(vars, data = x)
          pred <- stats::predict(fit, interval = "confidence", level = as.numeric(input$level))
          list(model = fit, predict = pred, response = x[[axis1()]])
        },
        simplify = FALSE
      )
    })

    ## Ellipses -----
    wrap <- reactive({
      req(x())
      level <- as.numeric(input$level)
      group <- if (isTruthy(extra_quali())) x()[[extra_quali()]] else NULL
      switch(
        input$wrap,
        tol = function(x, y, ...) dimensio::viz_tolerance(x, y, group = group, level = level, ...),
        conf = function(x, y, ...) dimensio::viz_confidence(x, y, group = group, level = level, ...),
        hull = function(x, y, ...) dimensio::viz_hull(x, y, group = group, ...),
        function(...) invisible()
      )
    })

    ## Graphical parameters -----
    param <- graphics_server("par")

    ## Build plot -----
    plot_scatter <- reactive({
      ## Select data
      req(x(), axis1(), axis2())

      col <- param$col_quali(x()[[extra_quali()]])
      pch <- param$pch(x()[[extra_quali()]])
      cex <- param$cex(x()[[extra_quanti()]])

      ## Build plot
      function() {
        graphics::plot(
          x = x()[[axis1()]],
          y = x()[[axis2()]],
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

        ## Add regression
        if (length(model()) > 0) {
          col_lines <- param$col_quali(names(model()))
          for (i in seq_along(model())) {
            fit <- model()[[i]]
            k <- order(fit$response)
            graphics::lines(x = fit$response[k], y = fit$predict[k, 1],
                            col = col_lines[i], lwd = 2)
          }
        }

        ## Add ellipses
        wrap()(x = x()[[axis1()]], y = x()[[axis2()]], color = param$pal_quali)

        ## Add legend
        if (isTruthy(extra_quali())) {
          graphics::legend(
            x = "topleft",
            legend = unique(x()[[extra_quali()]]),
            col = unique(col),
            pch = unique(pch)
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
