# UI ===========================================================================
#' Compositional Data Hierarchical Clustering UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [coda_hclust_server()]
#' @family coda modules
#' @keywords internal
#' @export
coda_hclust_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("HCLUST"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Hierarchical Clustering"),
        selectInput(
          inputId = ns("dist"),
          label = tr_("Distance"),
          choices = c(Aitchison = "euclidean")
        ),
        selectInput(
          inputId = ns("clust"),
          label = tr_("Clustering linkage"),
          choices = c("ward.D", "ward.D2", "single", "complete",
                      "average", "mcquitty", "median", "centroid"),
          selected = "ward.D2"
        ),
        bslib::input_task_button(id = ns("go"), label = tr_("(Re)Compute")),
        numericInput(
          inputId = ns("cut"),
          label = tr_("Desired number of clusters"),
          value = 1, min = 1, max = NA, step = 1
        ),
        downloadButton(
          outputId = ns("download_dist"),
          label = tr_("Download distances")
        ),
        downloadButton(
          outputId = ns("download_clust"),
          label = tr_("Download clusters")
        )
      ), # sidebar
      output_plot(
        id = ns("plot_dendro"),
        tools = graphics_ui(ns("par"), col_quant = FALSE, lty = FALSE, cex = FALSE),
        title = tr_("Dendrogram")
      ),
      border_radius = FALSE,
      fillable = TRUE
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Compositional Data Hierarchical Clustering Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`nexus::CompositionMatrix-class`] object.
#' @return
#'  No return value, called for side effects.
#' @seealso [coda_hclust_ui()]
#' @family coda modules
#' @keywords internal
#' @export
coda_hclust_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Check data -----
    old <- reactive({ x() }) |> bindEvent(input$go)
    notify_change(session$ns("change"), x, old, title = tr_("HCLUST"))

    ## Compute cluster -----
    compute_hclust <- ExtendedTask$new(
      function(x, method, clust) {
        mirai::mirai({
          clr <- nexus::transform_clr(x)
          d <- nexus::dist(clr, method = method)
          h <- stats::hclust(d, method = clust)
          h$dist <- d
          if (nexus::is_grouped(x)) h$groups <- nexus::group_names(x)
          h
        }, environment())
      }
    ) |>
      bslib::bind_task_button("go")

    observe({
      compute_hclust$invoke(x = x(), method = input$dist, clust = input$clust)
    }) |>
      bindEvent(input$go)

    results <- reactive({
      notify(compute_hclust$result(), title = tr_("Hierarchical Clustering"))
    })
    distances <- reactive({
      results()$dist
    })
    groups <- reactive({
      req(input$cut)
      stats::cutree(results(), k = input$cut)
    })

    ## Graphical parameters -----
    param <- graphics_server("par")

    ## Dendrogram -----
    plot_dendro <- reactive({
      req(results(), input$cut)

      function() {
        xlab <- sprintf(tr_("Aitchison distance, %s linkage"), results()$method)
        plot(results(), hang = -1, main = NULL, sub = "",
             xlab = xlab, ylab = "Height", las = 1)

        if (input$cut > 1) {
          stats::rect.hclust(results(), k = input$cut)
        }

        i <- results()$order
        g <- results()$groups
        if (!is.null(g)) {
          col <- param$col_quali(g)
          pch <- param$pch(g)
          graphics::points(
            x = seq_along(i),
            y = rep(0, length(i)),
            col = col[i],
            pch = pch[i]
          )

          arg <- list(x = "topright", pch = 16, bty = "n")
          leg <- stats::aggregate(
            data.frame(col = col, pch = pch),
            by = list(legend = g),
            FUN = unique
          )
          leg <- as.list(leg)
          leg <- utils::modifyList(leg, arg)
          do.call(graphics::legend, args = leg)
        }
      }
    })

    ## Render plot -----
    render_plot("plot_dendro", x = plot_dendro)

    ## Download -----
    output$download_dist <- export_table(distances, name = "distances")
    output$download_clust <- export_table(groups, name = "clusters")
  })
}
