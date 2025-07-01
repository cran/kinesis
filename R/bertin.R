# UI ===========================================================================
#' Bertin Diagrams UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @param title A [`character`] string giving the plot title.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [bertin_server()]
#' @family count data modules
#' @keywords internal
#' @export
bertin_ui <- function(id, title = NULL) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Plot"),
    layout_sidebar(
      sidebar = sidebar(
        ## Input: select plot
        radioButtons(
          inputId = ns("type"),
          label = tr_("Plot type"),
          selected = "ford",
          choiceNames = c(tr_("Ford diagram"), tr_("Bertin barplot"),
                          tr_("Bertin scalogram"), tr_("Heatmap")),
          choiceValues = c("ford", "barplot", "scalogram", "heatmap")
        ),
        conditionalPanel(
          condition = "input.type == 'ford'",
          ns = ns,
          checkboxInput(
            inputId = ns("eppm"),
            label = "EPPM",
            value = FALSE
          ),
          checkboxInput(
            inputId = ns("weights"),
            label = tr_("Weights"),
            value = FALSE
          ),
          helpText(info_article(author = "Desachy", year = "2004", doi = "10.3406/pica.2004.2396")),
        ),
        conditionalPanel(
          condition = "input.type == 'barplot'",
          ns = ns,
          radioButtons(
            inputId = ns("threshold"),
            label = tr_("Threshold"),
            selected = "none",
            choiceNames = c(tr_("None"), tr_("Mean"), tr_("Median")),
            choiceValues = c("none", "mean", "median")
          )
        )
      ),
      output_plot(
        id = ns("plot"),
        height = "100%",
        title = title,
        tools = graphics_ui(ns("par"), pch = FALSE, lty = FALSE, cex = FALSE)
      )
    )
  )
}

# Server =======================================================================
#' Bertin Diagrams Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @param verbose A [`logical`] scalar: should \R report extra information on
#'  progress?
#' @return
#'  No return value, called for side effects.
#' @seealso [bertin_ui()]
#' @family count data modules
#' @keywords internal
#' @export
bertin_server  <- function(id, x, verbose = get_option("verbose", FALSE)) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Get count data -----
    counts <- reactive({
      req(x())
      arkhe::keep_columns(x(), f = is.numeric, verbose = verbose)
    })

    ## Graphical parameters -----
    param <- graphics_server("par")

    ## Plot -----
    plot_permute <- reactive({
      req(counts())

      threshold <- switch(
        input$threshold,
        mean = mean,
        median = stats::median,
        none = NULL
      )

      switch(
        input$type,
        ford = function()
          tabula::plot_ford(counts(), weights = input$weights, EPPM = input$eppm),
        barplot = function()
          tabula::plot_bertin(counts(), threshold = threshold),
        scalogram = function()
          tabula::plot_spot(counts(), color = "black", legend = FALSE),
        heatmap = function()
          tabula::plot_heatmap(counts(), color = param$pal_quant, fixed_ratio = FALSE)
      )
    })

    ## Render plot -----
    render_plot("plot", x = plot_permute)
  })
}
