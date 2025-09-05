# UI ===========================================================================
#' Co-Occurrence UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [occurrence_server()]
#' @family count data modules
#' @keywords internal
#' @export
occurrence_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Co-Occurrence"),
    layout_sidebar(
      sidebar = sidebar(
        title = tr_("Co-Occurrence"),
        radioButtons(
          inputId = ns("method"),
          label = tr_("Method"),
          choiceNames = c(tr_("Absolute frequency"),
                          tr_("Relative frequency"),
                          tr_("Z-score")),
          choiceValues = c("absolute", "relative", "binomial")
        ),
        info_article(author = "Kintigh", year = "2006", doi = "10.6067/XCV8J38QSS"),
        bslib::input_task_button(id = ns("go"), label = tr_("(Re)Compute")),
        downloadButton(
          outputId = ns("download"),
          label = tr_("Download results")
        )
      ), # sidebar
      layout_columns(
        col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6)),
        output_plot(
          id = ns("plot"),
          tools = graphics_ui(ns("par"), col_quali = FALSE,
                              pch = FALSE, lty = FALSE, cex = FALSE),
        ),
        card(
          gt::gt_output(outputId = ns("table"))
        )
      )
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Co-Occurrence Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` returned by [diversity_server()].
#' @param verbose A [`logical`] scalar: should \R report extra information on
#'  progress?
#' @return
#'  No return value, called for side effects.
#' @seealso [occurrence_ui()]
#' @family count data modules
#' @keywords internal
#' @export
occurrence_server <- function(id, x, verbose = get_option("verbose", FALSE)) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Check data -----
    old <- reactive({ x() }) |> bindEvent(input$go)
    notify_change(session$ns("change"), x, old, title = tr_("Co-Occurrence"))

    ## Compute index -----
    compute_occur <- ExtendedTask$new(
      function(x, method) {
        mirai::mirai({ tabula::occurrence(x, method = tolower(method)) }, environment())
      }
    ) |>
      bslib::bind_task_button("go")

    observe({
      compute_occur$invoke(x = x(), method = input$method)
    }) |>
      bindEvent(input$go)

    results <- reactive({
      notify(compute_occur$result(), title = tr_("Co-Occurrence"))
    })

    ## Graphical parameters -----
    param <- graphics_server("par")

    ## Plot -----
    map <- reactive({
      req(results())
      function() tabula::plot_heatmap(results(), color = param$pal_quant)
    })

    ## Render table -----
    output$table <- gt::render_gt({
      req(results())
      tbl <- as.data.frame(as.matrix(results()))
      gt::gt(tbl, rownames_to_stub = TRUE) |>
        gt::tab_options(table.width = "100%")
    })

    ## Render plot -----
    render_plot("plot", x = map)

    ## Download -----
    output$download <- export_table(results, "occurrence")

    results
  })
}
