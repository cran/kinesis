# UI ===========================================================================
#' Ternary Plot UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [ternary_server()]
#' @family plot modules
#' @keywords internal
#' @export
ternary_ui <- function(id) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Ternary Plot"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Ternary Plot"),
        accordion(
          accordion_panel(
            title = tr_("Variables"),
            ## Input: select axes
            selectize_ui(id = ns("axis1"), label = tr_("Component X")),
            selectize_ui(id = ns("axis2"), label = tr_("Component Y")),
            selectize_ui(id = ns("axis3"), label = tr_("Component Z")),
            ## Input: aesthetics mapping
            selectize_ui(id = ns("extra_quali"), label = tr_("Extra qualitative variable")),
            selectize_ui(id = ns("extra_quanti"), label = tr_("Extra quantitative variable")),
          ),
          accordion_panel(
            title = tr_("Layers"),
            ## Input: add points
            checkboxInput(
              inputId = ns("points"),
              label = tr_("Show points"),
              value = TRUE
            ),
            ## Input: add density
            checkboxInput(
              inputId = ns("density"),
              label = tr_("Density contour"),
              value = FALSE
            ),
            radioButtons(
              inputId = ns("tile"),
              label = tr_("Heatmap"),
              choiceNames = c(tr_("None"), tr_("Bin"), tr_("Density")),
              choiceValues = c("none", "bin", "dens"),
              selected = "none"
            ),
            sliderInput(
              inputId = ns("bin"),
              label = tr_("Number of bins"),
              min = 5, max = 20,
              value = 10, step = 1
            )
          ),
          accordion_panel(
            title = tr_("Transform"),
            checkboxInput(
              inputId = ns("center"),
              label = tr_("Center"),
              value = FALSE
            ),
            checkboxInput(
              inputId = ns("scale"),
              label = tr_("Scale"),
              value = FALSE
            )
          ),
          accordion_panel(
            title = tr_("Envelopes"),
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
            )
          ),
          accordion_panel(
            title = tr_("Annotations"),
            ## Input: add a grid
            checkboxInput(
              inputId = ns("grid"),
              label = tr_("Grid"),
              value = TRUE
            ),
            ## Input: add labels
            checkboxInput(
              inputId = ns("labels"),
              label = tr_("Labels"),
              value = FALSE
            )
            ## Input: add a legend
            # TODO
            # checkboxInput(
            #   inputId = ns("legend"),
            #   label = tr_("Legend"),
            #   value = TRUE
            # )
          )
        )
      ), # sidebar
      helpText(
        tr_("Visualize your data in the ternary space."),
        tr_("Click and drag to select an area, then double-click to zoom in."),
        tr_("Double-click again to reset the zoom.")
      ),
      output_plot(
        id = ns("ternplot"),
        tools = graphics_ui(ns("par"), lty = FALSE),
        title = tr_("Plot"),
        dblclick = ns("ternplot_dblclick"),
        brush = brushOpts(
          id = ns("ternplot_brush"),
          resetOnNew = TRUE
        ),
        height = "100%"
      )
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Ternary Plot Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `matrix`-like object.
#' @return
#'  No return value, called for side effects.
#' @seealso [ternary_ui()]
#' @family plot modules
#' @keywords internal
#' @export
ternary_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Select columns -----
    data_raw <- reactive({
      as.data.frame(x())
    })
    quanti <- reactive({
      req(data_raw())
      i <- which(arkhe::detect(x = data_raw(), f = is.numeric, margin = 2))
      colnames(data_raw())[i]
    })
    quali <- reactive({
      req(data_raw())
      i <- which(arkhe::detect(x = data_raw(), f = Negate(is.numeric), margin = 2))
      colnames(data_raw())[i]
    })
    axis1 <- update_selectize_values("axis1", x = quanti)
    axis2 <- update_selectize_values("axis2", x = quanti, exclude = axis1)
    axis12 <- reactive({ c(axis1(), axis2()) })
    axis3 <- update_selectize_values("axis3", x = quanti, exclude = axis12)
    extra_quali <- update_selectize_values("extra_quali", x = quali)
    extra_quanti <- update_selectize_values("extra_quanti", x = quanti)

    ## Interactive zoom -----
    ## When a double-click happens, check if there's a brush on the plot.
    ## If so, zoom to the brush bounds; if not, reset the zoom.
    range_ternplot <- reactiveValues(x = NULL, y = NULL)
    observe({
      range_ternplot$x <- brush_xlim(input$ternplot_brush)
      range_ternplot$y <- brush_ylim(input$ternplot_brush)
    }) |>
      bindEvent(input$ternplot_dblclick)

    ## Get ternary data -----
    data_tern <- reactive({
      req(data_raw(), axis1(), axis2(), axis3())
      tern <- data_raw()[, c(axis1(), axis2(), axis3())]
      tern[rowSums(tern, na.rm = TRUE) != 0, , drop = FALSE]
    })

    ## Graphical parameters -----
    param <- graphics_server("par")

    ## Build plot -----
    plot_ternary <- reactive({
      ## Select data
      req(data_tern())
      tern <- data_tern()
      n <- nrow(tern)

      ## Compute center and scale
      no_scale <- isFALSE(input$center) && isFALSE(input$scale)

      ## Graphical parameters
      if (isTruthy(extra_quali())) {
        symbol_group <- data_raw()[[extra_quali()]]
        col <- param$col_quali(symbol_group)
      } else {
        symbol_group <- rep("", n)
        col <- param$col_quant(data_raw()[[extra_quanti()]])
      }
      pch <- param$pch(data_raw()[[extra_quali()]])
      cex <- param$cex(data_raw()[[extra_quanti()]])

      ## Window
      range_coord <- list(x = NULL, y = NULL, z = NULL)
      if (isTruthy(range_ternplot$x) && isTruthy(range_ternplot$y)) {
        x_pts <- c(range_ternplot$x, mean(range_ternplot$x))
        y_pts <- c(range_ternplot$y, sqrt(3) * diff(range_ternplot$y) / 2)
        range_coord <- isopleuros::coordinates_cartesian(x = x_pts, y = y_pts)
      }

      ## Heatmap
      bin <- as.numeric(input$bin)
      fun_tile <- switch(
        input$tile,
        bin = isopleuros::tile_bin(tern),
        dens = isopleuros::tile_density(tern),
        NULL
      )

      ## Envelope
      level <- as.numeric(input$level)
      fun_wrap <- switch(
        input$wrap,
        tol = function(x, ...) isopleuros::ternary_tolerance(x, level = level, ...),
        conf = function(x, ...) isopleuros::ternary_confidence(x, level = level, ...),
        hull = function(x, ...) isopleuros::ternary_hull(x, ...),
        function(...) invisible()
      )

      ## Build plot
      function() {
        oldpar <- graphics::par(mar = c(1, 1, 1, 1), no.readonly = TRUE)
        on.exit(graphics::par(oldpar))

        z <- isopleuros::ternary_plot(
          x = tern,
          type = "n",
          xlim = range_coord$x,
          ylim = range_coord$y,
          zlim = range_coord$z,
          xlab = axis1(),
          ylab = axis2(),
          zlab = axis3(),
          center = input$center,
          scale = input$scale
        )

        ## Add grid
        if (isTRUE(input$grid)) {
          isopleuros::ternary_grid(center = z$center, scale = z$scale)
        }

        if (no_scale) {
          ## Heatmap
          if (isTruthy(fun_tile)) {
            isopleuros::ternary_image(
              f = fun_tile,
              n = bin,
              palette = param$col_quant
            )
          }

          ## Density contours
          if (isTRUE(input$density)) {
            isopleuros::ternary_density(tern)
          }

          ## Envelope
          for (i in split(seq_len(n), f = symbol_group)) {
            z <- tern[i, , drop = FALSE]
            if (nrow(z) < 3) next
            fun_wrap(z, lty = 1, border = col[i])
          }
        }

        ## Add points
        if (isTRUE(input$points)) {
          isopleuros::ternary_points(tern, col = col, pch = pch, cex = cex,
                                     center = z$center, scale = z$scale)
        }

        ## Add labels
        if (isTRUE(input$labels)) {
          isopleuros::ternary_labels(tern, center = z$center, scale = z$scale,
                                     labels = rownames(tern), col = col)
        }

        ## Add legend
        if (isTruthy(extra_quali())) {
          graphics::legend(
            x = "topleft",
            legend = unique(symbol_group),
            col = unique(col),
            pch = unique(pch),
            bty = "n"
          )
        }
      }
    })

    ## Render plot -----
    render_plot("ternplot", x = plot_ternary)
  })
}
