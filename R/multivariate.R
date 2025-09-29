# UI ===========================================================================
#' Mutlivariate Analysis UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return A tab that can be passed to [shiny::tabsetPanel()].
#' @return
#'  A navigation container (see [bslib::navset_card_pill()]).
#' @seealso [multivariate_server()]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
multivariate_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  navset_card_pill(
    sidebar = sidebar(
      title = tr_("Factor maps"),
      ## Input: display options
      selectizeInput(
        inputId = ns("axis1"),
        label = tr_("Horizontal axis"),
        choices = NULL,
        multiple = FALSE
      ),
      selectizeInput(
        inputId = ns("axis2"),
        label = tr_("Vertical axis"),
        choices = NULL,
        multiple = FALSE
      ),
      checkboxInput(
        inputId = ns("lab_ind"),
        label = tr_("Label individuals"),
        value = FALSE
      ),
      checkboxInput(
        inputId = ns("lab_var"),
        label = tr_("Label variables"),
        value = TRUE
      ),
      checkboxInput(
        inputId = ns("sup_ind"),
        label = tr_("Display supplementary individuals"),
        value = TRUE
      ),
      checkboxInput(
        inputId = ns("sup_var"),
        label = tr_("Display supplementary variables"),
        value = TRUE
      ),
      selectize_ui(
        id = ns("extra_quanti"),
        label = tr_("Extra quantitative variable")
      ),
      selectize_ui(
        id = ns("extra_quali"),
        label = tr_("Extra qualitative variable")
      ),
      ## Input: add ellipses
      radioButtons(
        inputId = ns("wrap"),
        label = tr_("Wrap:"),
        choiceNames = c(tr_("None"), tr_("Tolerance ellipse"),
                        tr_("Confidence ellipse"), tr_("Convex hull")),
        choiceValues = c("none", "tolerance", "confidence", "hull"),
      ),
      checkboxGroupInput(
        inputId = ns("ellipse_level"),
        label = tr_("Ellipse level:"),
        selected = "0.95",
        choiceNames = c("68%", "95%", "99%"),
        choiceValues = c("0.68", "0.95", "0.99")
      )
      # TODO: legend
    ),
    ## Results -----
    nav_panel(
      title = tr_("Results"),
      helpText(
        tr_("Click and drag to select an area, then double-click to zoom in."),
        tr_("Double-click again to reset the zoom.")
      ),
      layout_column_wrap(
        output_plot(
          id = ns("plot_ind"),
          tools = graphics_ui(ns("par_ind"), lty = FALSE),
          title = tr_("Individuals factor map"),
          dblclick = ns("plot_ind_dblclick"),
          brush = brushOpts(
            id = ns("plot_ind_brush"),
            resetOnNew = TRUE
          ),
          height = "100%"
        ),
        output_plot(
          id = ns("plot_var"),
          tools = graphics_ui(ns("par_var"), col_quant = FALSE, pch = FALSE, lty = FALSE, cex = FALSE),
          title = tr_("Variables factor map"),
          dblclick = ns("plot_var_dblclick"),
          brush = brushOpts(
            id = ns("plot_var_brush"),
            resetOnNew = TRUE
          ),
          height = "100%"
        )
      ) # layout_columns
    ),
    ## Individuals -----
    nav_panel(
      title = tr_("Individuals"),
      layout_column_wrap(
        output_plot(id = ns("plot_cos2_1")),
        output_plot(id = ns("plot_cos2_2")),
        min_height = "50%"
      ),
      gt::gt_output(outputId = ns("info_ind"))
    ),
    ## Variables -----
    nav_panel(
      title = tr_("Variables"),
      layout_column_wrap(
        output_plot(id = ns("plot_contrib_1")),
        output_plot(id = ns("plot_contrib_2")),
        min_height = "50%"
      ),
      gt::gt_output(outputId = ns("info_var"))
    ),
    ## Screeplot -----
    nav_panel(
      title = tr_("Screeplot"),
      layout_column_wrap(
        output_plot(id = ns("screeplot"), title = tr_("Screeplot")),
        tableOutput(outputId = ns("variance"))
      )
    )
  )
}

# Server =======================================================================
#' Multivariate Analysis Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`dimensio::MultivariateAnalysis-class`] object.
#' @param y A reactive `matrix`-like object use to compute the multivariate
#'  analysis.
#' @return
#'  No return value, called for side effects.
#' @seealso [multivariate_ui]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
multivariate_server <- function(id, x, y) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Illustrative variables -----
    ## Set group_var for nexus::GroupedComposition objects
    extra <- reactive({ as.data.frame(y(), group_var = tr_("Group")) })
    quanti <- subset_quantitative(extra)
    quali <- subset_qualitative(extra)

    col_quali <- update_selectize_colnames("extra_quali", x = quali)
    col_quanti <- update_selectize_colnames("extra_quanti", x = quanti)

    ## Extra variables -----
    extra_quali <- select_data(quali, col_quali, drop = TRUE)
    extra_quanti <- select_data(quanti, col_quanti, drop = TRUE)

    ## Eigenvalues -----
    eigen <- reactive({
      req(x())
      dimensio::get_eigenvalues(x())
    })

    ## Update UI -----
    axes <- reactive({
      choices <- seq_len(nrow(eigen()))
      names(choices) <- rownames(eigen())
      choices
    })
    observe({
      freezeReactiveValue(input, "axis1")
      updateSelectizeInput(inputId = "axis1", choices = axes())
    }) |>
      bindEvent(axes())
    observe({
      choices <- axes()[-axis1()]
      freezeReactiveValue(input, "axis2")
      updateSelectizeInput(inputId = "axis2", choices = choices)
    }) |>
      bindEvent(axis1())

    ## Select axes -----
    axis1 <- reactive({
      req(input$axis1)
      as.numeric(input$axis1)
    })
    axis2 <- reactive({
      req(input$axis2)
      as.numeric(input$axis2)
    })

    ## Graphical parameters -----
    param_ind <- graphics_server("par_ind")
    param_var <- graphics_server("par_var")

    ## Plot -----
    ## Interactive zoom
    ## When a double-click happens, check if there's a brush on the plot.
    ## If so, zoom to the brush bounds; if not, reset the zoom.
    range_ind <- reactiveValues(x = NULL, y = NULL)
    range_var <- reactiveValues(x = NULL, y = NULL)
    observe({
      range_ind$x <- brush_xlim(input$plot_ind_brush)
      range_ind$y <- brush_ylim(input$plot_ind_brush)
    }) |>
      bindEvent(input$plot_ind_dblclick)

    observe({
      range_var$x <- brush_xlim(input$plot_var_brush)
      range_var$y <- brush_ylim(input$plot_var_brush)
    }) |>
      bindEvent(input$plot_var_dblclick)

    ## Individuals
    plot_ind <- reactive({
      req(x())

      default_quali <- "observation"
      if (!isTruthy(extra_quali()) && isTruthy(extra_quanti())) {
        default_quali <- NULL
        col <- param_ind$pal_quanti
      } else {
        col <- param_ind$pal_quali
      }

      add_ellipses <- any(input$wrap %in% c("confidence", "tolerance"))
      add_hull <- isTRUE(input$wrap == "hull")

      function() {
        dimensio::viz_rows(
          x = x(),
          axes = c(axis1(), axis2()),
          active = TRUE,
          sup = isTRUE(input$sup_ind),
          labels = isTRUE(input$lab_ind),
          extra_quali = extra_quali() %|||% default_quali,
          extra_quanti = extra_quanti(),
          color = col,
          symbol = param_ind$pal_pch,
          size = param_ind$pal_cex,
          xlim = range_ind$x,
          ylim = range_ind$y,
          panel.first = graphics::grid()
        )

        if (add_ellipses) {
          dimensio::viz_ellipses(
            x = x(),
            group = extra_quali(),
            type = input$wrap,
            level = as.numeric(input$ellipse_level),
            color = param_ind$pal_quali
          )
        }
        if (add_hull) {
          dimensio::viz_hull(
            x = x(),
            group = extra_quali(),
            color = param_ind$pal_quali
          )
        }
      }
    })

    ## Variables
    plot_var <- reactive({
      req(x())

      function() {
        dimensio::viz_variables(
          x = x(),
          axes = c(axis1(), axis2()),
          active = TRUE,
          sup = isTRUE(input$sup_var),
          labels = isTRUE(input$lab_var),
          extra_quali = "observation",
          color = param_var$pal_quali,
          symbol = c(1, 3),
          xlim = range_var$x,
          ylim = range_var$y,
          panel.first = graphics::grid()
        )
      }
    })

    plot_cos2_1 <- reactive({
      req(x())
      function() {
        dimensio::viz_cos2(x = x(), margin = 1, axes = axis1())
      }
    })

    plot_cos2_2 <- reactive({
      req(x())
      function() {
        dimensio::viz_cos2(x = x(), margin = 1, axes = axis2())
      }
    })

    plot_contrib_1 <- reactive({
      req(x())
      function() {
        dimensio::viz_contributions(x = x(), margin = 2, axes = axis1())
      }
    })

    plot_contrib_2 <- reactive({
      req(x())
      function() {
        dimensio::viz_contributions(x = x(), margin = 2, axes = axis2())
      }
    })

    plot_eigen <- reactive({
      req(x())
      function() {
        dimensio::screeplot(
          x = x(),
          cumulative = TRUE,
          labels = FALSE,
          limit = sum(eigen()[, 3] <= 99)
        )
      }
    })

    ## Render plots -----
    render_plot("plot_ind", x = plot_ind)
    render_plot("plot_var", x = plot_var)
    render_plot("plot_cos2_1", x = plot_cos2_1)
    render_plot("plot_cos2_2", x = plot_cos2_2)
    render_plot("plot_contrib_1", x = plot_contrib_1)
    render_plot("plot_contrib_2", x = plot_contrib_2)
    render_plot("screeplot", x = plot_eigen)

    ## Render tables -----
    output$variance <- gt::render_gt({
      gt::gt(eigen(), rownames_to_stub = TRUE) |>
        gt::tab_options(table.width = "100%") |>
        gt::fmt_number(
          columns = c("eigenvalues"),
          decimals = 3
        ) |>
        gt::fmt_percent(
          columns = c("variance", "cumulative"),
          scale_values = FALSE
        ) |>
        gt::cols_label(
          eigenvalues = tr_("Eigenvalues"),
          variance = tr_("Explained var. (%)"),
          cumulative = tr_("Cumulative var. (%)")
        )
    })

    output$info_ind <- gt::render_gt({
      req(x())
      multivariate_summary(x(), axes = c(axis1(), axis2()), margin = 1)
    })
    output$info_var <- gt::render_gt({
      req(x())
      multivariate_summary(x(), axes = c(axis1(), axis2()), margin = 2)
    })
  })
}

multivariate_summary <- function(x, axes, margin) {
  dimensio::summary(x, axes = axes, margin = margin) |>
    as.data.frame() |>
    gt::gt(rownames_to_stub = TRUE) |>
    gt::fmt_number(decimals = 3) |>
    gt::tab_spanner(
      label = tr_("Coordinates"),
      columns = gt::ends_with("coord"),
      id = "coord"
    ) |>
    gt::tab_spanner(
      label = tr_("Contribution"),
      columns = gt::ends_with("contrib"),
      id = "contrib"
    ) |>
    gt::tab_spanner(
      label = tr_("Squared cosinus"),
      columns = gt::ends_with("cos2"),
      id = "cos2"
    ) |>
    gt::cols_label_with(
      columns = gt::matches("dist"),
      fn = function(x) tr_("Distance")
    ) |>
    gt::cols_label_with(
      columns = gt::matches("inertia"),
      fn = function(x) tr_("Inertia")
    ) |>
    gt::cols_label_with(
      columns = gt::starts_with("F"),
      fn = function(x) {
        paste(tr_("Axis"), regmatches(x, regexpr("[0-9]", x)), sep = " ")
      }
    ) |>
    gt::opt_interactive(
      use_compact_mode = TRUE,
      use_page_size_select = TRUE
    )
}
