# UI ===========================================================================
#' Compositional Data Summary UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [coda_summary_server()]
#' @family coda modules
#' @keywords internal
#' @export
coda_summary_ui <- function(id) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Statistics"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Descriptive Statistics"),
        helpText(tr_("Data summary and descriptive statistics.")),
        downloadButton(
          outputId = ns("download"),
          label = tr_("Download tables")
        )
      ),
      navset_card_pill(
        placement = "above",
        nav_panel(
          title = tr_("Location"),
          gt::gt_output(outputId = ns("mean")),
          gt::gt_output(outputId = ns("quantile"))
        ),
        nav_panel(
          title = tr_("Covariance"),
          gt::gt_output(outputId = ns("covariance"))
        ),
        nav_panel(
          title = tr_("PIP"),
          gt::gt_output(outputId = ns("pip"))
        ),
        nav_panel(
          title = tr_("Variation Matrix"),
          navset_pill(
            nav_panel(
              title = tr_("Table"),
              class = "pt-3",
              gt::gt_output(outputId = ns("variation"))
            ),
            nav_panel(
              title = tr_("Heatmap"),
              class = "pt-3",
              output_plot(id = ns("heatmap"))
            ),
            nav_panel(
              title = tr_("Dendrogram"),
              class = "pt-3",
              output_plot(id = ns("dendrogram"))
            )
          )
        )
      ) # navset_card_underline
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Compositional Data Summary Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`nexus::CompositionMatrix-class`] object.
#' @return
#'  No return value, called for side effects.
#' @seealso [coda_summary_ui()]
#' @family coda modules
#' @keywords internal
#' @export
coda_summary_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Location -----
    data_loc <- reactive({
      req(x())
      if (nexus::is_grouped(x())) {
        nexus::aggregate(x(), FUN = nexus::mean, na.rm = FALSE)
      } else {
        m <- nexus::mean(x(), na.rm = FALSE)
        matrix(m, nrow = 1, dimnames = list("center", names(m)))
      }
    })

    ## Spread -----
    # TODO

    ## Percentiles -----
    data_quant <- reactive({
      req(x())
      nexus::quantile(x(), probs = seq(0, 1, 0.25))
    })

    ## CLR covariance -----
    data_cov <- reactive({
      req(x())
      nexus::covariance(x(), center = TRUE)
    })

    ## PIP -----
    data_pip <- reactive({
      req(x())
      nexus::pip(x())
    })

    ## Variation matrix -----
    data_var <- reactive({
      req(x())
      nexus::variation(x())
    })

    ## Clustering -----
    plot_clust <- reactive({
      d <- stats::as.dist(data_var())
      h <- stats::hclust(d, method = "ward.D2")

      function() {
        plot(h, hang = -1, main = "", sub = "",
             xlab = "", ylab = tr_("Total variation"), las = 1)
      }
    })

    ## Heatmap -----
    plot_heatmap <- reactive({
      req(data_var())
      function() tabula::plot_heatmap(data_var(), fixed_ratio = TRUE)
    })

    Aitchison1986 <- info_article(
      author = "Aitchison", year = "1986", html = FALSE
    )
    Egozcue2023 <- info_article(
      author = "Egozcue & Pawlowsky-Glahn", year = "2023",
      doi = "10.57645/20.8080.02.7", html = FALSE
    )

    ## Render table -----
    output$mean <- gt::render_gt({
      req(x(), data_loc())
      data_loc() |>
        as.data.frame() |>
        gt::gt(rownames_to_stub = nexus::is_grouped(x())) |>
        gt::fmt_percent(decimals = 3) |>
        gt::sub_missing() |>
        gt::tab_header(title = tr_("Compositional Mean"))
    })
    output$quantile <-  gt::render_gt({
      req(data_quant())
      data_quant() |>
        as.data.frame() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::fmt_percent(decimals = 3) |>
        gt::sub_missing() |>
        gt::tab_header(title = tr_("Percentile Table"))
    })
    output$covariance <- gt::render_gt({
      req(data_cov())
      covar <- data_cov()
      covar[lower.tri(covar, diag = FALSE)] <- NA

      covar |>
        as.data.frame() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::fmt_number(decimals = 3) |>
        gt::sub_missing(missing_text = "") |>
        gt::tab_header(title = tr_("Centered Log-Ratio Covariance")) |>
        gt::tab_source_note(source_note = gt::html(Aitchison1986))
    })
    output$pip <- gt::render_gt({
      req(data_pip())
      prop <- data_pip()
      prop[lower.tri(prop, diag = TRUE)] <- NA

      prop |>
        as.data.frame() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::fmt_number(decimals = 3) |>
        gt::sub_missing(missing_text = "") |>
        gt::tab_style_body(
          fn = function(x) x >= 0.75,
          style = gt::cell_fill(color = "#FFAABB")
        ) |>
        gt::tab_header(title = tr_("Proportionality Index of Parts")) |>
        gt::tab_source_note(source_note = gt::html(Egozcue2023))
    })
    output$variation <- gt::render_gt({
      req(data_var())
      varia <- data_var()
      varia[lower.tri(varia, diag = TRUE)] <- NA

      varia |>
        as.data.frame() |>
        gt::gt(rownames_to_stub = TRUE) |>
        gt::fmt_number(decimals = 3) |>
        gt::sub_missing(missing_text = "") |>
        gt::tab_header(title = tr_("Variation Matrix")) |>
        gt::tab_source_note(source_note = gt::html(Aitchison1986))
    })

    ## Render plot -----
    render_plot("heatmap", x = plot_heatmap)
    render_plot("dendrogram", x = plot_clust)

    ## Download -----
    output$download <- export_multiple(
      mean = data_loc,
      quantiles = data_quant,
      covariance = data_cov,
      variation = data_var,
      pip = data_pip,
      name = "coda_summary"
    )
  })
}
