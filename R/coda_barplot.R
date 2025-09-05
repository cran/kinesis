# UI ===========================================================================
#' Compositional Bar Plot UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [coda_barplot_server()]
#' @family coda modules
#' @keywords internal
#' @export
coda_barplot_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Bar Plot"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Bar Plot"),
        checkboxInput(
          inputId = ns("select_major"),
          label = tr_("Major elements"),
          value = TRUE
        ),
        checkboxInput(
          inputId = ns("select_minor"),
          label = tr_("Minor elements"),
          value = TRUE
        ),
        checkboxInput(
          inputId = ns("select_trace"),
          label = tr_("Trace elements"),
          value = TRUE
        ),
        hr(),
        checkboxInput(
          inputId = ns("order_columns"),
          label = tr_("Sort columns"),
          value = FALSE
        ),
        selectize_ui(id = ns("order_rows"), label = tr_("Row order")),
        checkboxInput(
          inputId = ns("decreasing"),
          label = tr_("Decreasing row order"),
          value = FALSE
        )
      ), # sidebar
      output_plot(
        id = ns("plot"),
        tools = list(
          graphics_ui(ns("par"), col_quant = FALSE, pch = FALSE, lty = FALSE, cex = FALSE),
          numericInput(
            inputId = ns("space"),
            label = tr_("Gutter"),
            value = 0.2,
            min = 0, max = 0.5, step = 0.1
          )
        ),
        height = "100%",
        title = tr_("Plot")
      )
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Bar Plot Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`nexus::CompositionMatrix-class`] object.
#' @return
#'  No return value, called for side effects.
#' @seealso [coda_barplot_ui()]
#' @family coda modules
#' @keywords internal
#' @export
coda_barplot_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Subset -----
    data_bar <- reactive({
      req(x())

      elements <- logical(ncol(x()))
      is_major <- nexus::is_element_major(x())
      is_minor <- nexus::is_element_minor(x())
      is_trace <- nexus::is_element_trace(x())

      elements[which(is_major)] <- isTRUE(input$select_major)
      elements[which(is_minor)] <- isTRUE(input$select_minor)
      elements[which(is_trace)] <- isTRUE(input$select_trace)

      z <- x()[, which(elements), drop = FALSE]
      validate_dim(z, j = 3)
      z
    })

    ## Select column -----
    col_bar <- update_selectize_colnames("order_rows", x = data_bar)

    ## Graphical parameters -----
    param <- graphics_server("par")

    ## Build barplot -----
    plot_bar <- reactive({
      req(data_bar())

      col <- notify({
        pal <- khroma::palette_color_discrete(param$pal_quali, domain = colnames(x()))
        pal(colnames(data_bar()))
      })

      function() {
        nexus::barplot(
          height = data_bar(),
          order_columns = isTRUE(input$order_columns),
          order_rows = col_bar() %|||% NULL,
          decreasing = isTRUE(input$decreasing),
          color = col,
          space = input$space %|||% 0
        )
      }
    })

    ## Render barplot -----
    render_plot("plot", x = plot_bar)
  })
}
