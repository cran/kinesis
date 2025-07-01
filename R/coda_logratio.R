# UI ===========================================================================
#' Log-Ratio UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @param title A [`character`] string giving the sidebar title.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [logratio_server()]
#' @family coda modules
#' @keywords internal
#' @export
logratio_ui <- function(id, title) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = title,
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = textOutput(
          outputId = ns("title"),
          container = function(...) tags$header(..., class = "sidebar-title")
        ),
        helpText(tr_("Compute log-ratio transformation of compositional data.")),
        checkboxInput(
          inputId = ns("weights"),
          label = tr_("Weighted log-ratio"),
          value = FALSE
        ),
        uiOutput(outputId = ns("settings")),
        radioButtons(
          inputId = ns("type"),
          label = tr_("Plot type"),
          selected = "scatter",
          choiceNames = c(tr_("Scatter plot"), tr_("Boxplot")),
          choiceValues = c("scatter", "boxplot")
        ),
        downloadButton(outputId = ns("download_table"),
                       label = tr_("Download log-ratio")),
        ## Output: graph
        plotOutput(outputId = ns("graph"))
      ), # sidebar
      ## Output: plot
      output_plot(
        id = ns("plot"),
        tools = graphics_ui(ns("par"), col_quant = FALSE, lty = FALSE, cex = FALSE),
        height = "100%",
        title = tr_("Density")
      )
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Log-Ratio Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`nexus::CompositionMatrix-class`] object.
#' @param method A [`character`] string specifying the log-ratio to be computed.
#' @return A reactive [`nexus::LogRatio-class`] object.
#' @seealso [logratio_ui()]
#' @family coda modules
#' @keywords internal
#' @export
logratio_server <- function(id, x, method) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Render settings -----
    output$settings <- renderUI({
      if (!(method == "alr" || method == "plr")) return(NULL)
      label <- switch (
        method,
        alr = tr_("Rationing part:"),
        plr = tr_("Pivotal variable:")
      )
      selectizeInput(
        inputId = session$ns("pivot"),
        label = label,
        choices = colnames(x()),
        selected = NULL,
        multiple = FALSE,
      )
    })

    ## Compute -----
    logratio <- reactive({
      req(x())

      pivot <- input$pivot %|||% 1
      trans <- switch (
        method,
        clr = function(x) nexus::transform_clr(x, weights = input$weights),
        alr = function(x) nexus::transform_alr(x, j = pivot, weights = input$weights),
        ilr = function(x) nexus::transform_ilr(x),
        plr = function(x) nexus::transform_plr(x, pivot = pivot)
      )

      notify(trans(x()), title = toupper(method))
    })

    ## Graphical parameters -----
    param <- graphics_server("par")

    ## Plot -----
    plot_log <- reactive({
      req(logratio())

      lvl <- ""
      if (nexus::is_grouped(logratio())) lvl <- nexus::group_levels(logratio())
      fun <- switch(
        input$type,
        scatter = function(x)
          plot(x, color = param$col_quali(lvl), symbol = param$pch(lvl)),
        boxplot = function(x)
          nexus::boxplot(x, color = param$col_quali(lvl))
      )

      function() {
        fun(logratio())
      }
    })

    ## Graph -----
    plot_graph <- reactive({
      req(logratio())
      if (inherits(logratio(), "CLR")) return(NULL)
      graph <- nexus::as_graph(logratio())

      plot(graph)
      grDevices::recordPlot()
    })

    ## Render title -----
    output$title <- renderText({
      switch(
        method,
        clr = tr_("Centered Log-Ratio"),
        alr = tr_("Additive Log-Ratio"),
        ilr = tr_("Isometric Log-Ratio"),
        plr = tr_("Pivot Log-Ratio"),
        ""
      )
    })

    ## Render table -----
    # TODO?

    ## Render plot -----
    render_plot("plot", x = plot_log)

    ## Render graph -----
    output$graph <- renderPlot({
      req(plot_graph())
      grDevices::replayPlot(plot_graph())
    })

    ## Download -----
    output$download_table <- export_table(logratio, name = paste0("coda_", method))

    logratio
  })
}
