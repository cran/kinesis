# UI ===========================================================================
#' Beta Diversity UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [diversity_beta_server()]
#' @family count data modules
#' @keywords internal
#' @export
diversity_beta_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = HTML(tr_("&#946; Diversity")),
    layout_sidebar(
      sidebar = sidebar(
        title = tr_("Principal Coordinates Analysis"),
        selectInput(
          inputId = ns("method"),
          label = tr_("Dissimilarity measure"),
          # TODO: change 'sorenson' to 'sorensen'
          choices = c(`Bray-Curtis` = "bray", `Dice-Sorensen` = "sorenson",
                      `Morisita-Horn` = "morisita"),
          multiple = FALSE
        ),
        bslib::input_task_button(id = ns("go"), label = tr_("(Re)Compute")),
        downloadButton(
          outputId = ns("download_beta"),
          label = tr_("Download dissimilarity matrix")
        ),
        downloadButton(
          outputId = ns("download_pcoa"),
          label = tr_("Download PCoA results")
        ),
        hr(),
        checkboxInput(
          inputId = ns("pcoa_labels"),
          label = tr_("Display labels"),
          value = FALSE
        ),
        ## Input: variable mapping
        selectize_ui(
          id = ns("extra_quanti"),
          label = tr_("Alpha diversity")
        ),
        selectize_ui(
          id = ns("extra_quali"),
          label = tr_("Groups")
        ),
        checkboxInput(
          inputId = ns("hull"),
          label = tr_("Convex hull"),
          value = FALSE
        )
      ), # sidebar
      layout_columns(
        col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6)),
        output_plot(
          id = ns("plot_diss"),
          title = tr_("Dissimilarity"),
          tools = graphics_ui(ns("par_diss"), col_quali = FALSE, pch = FALSE, lty = FALSE, cex = FALSE),
          height = "100%"
        ),
        output_plot(
          id = ns("plot_pcoa"),
          title = tr_("PCoA"),
          tools = graphics_ui(ns("par_pcoa"), lty = FALSE),
          height = "100%"
        )
      )
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Beta Diversity Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` returned by [diversity_server()].
#' @param quanti A reactive `data.frame` returned by [diversity_alpha_server()].
#' @param quali A reactive `data.frame` (typically returned by [import_server()]).
#' @return
#'  No return value, called for side effects.
#' @seealso [diversity_beta_ui()]
#' @family count data modules
#' @keywords internal
#' @export
diversity_beta_server <- function(id, x, quanti, quali) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(quanti))
  stopifnot(is.reactive(quali))

  moduleServer(id, function(input, output, session) {
    ## Update UI -----
    col_quali <- update_selectize_colnames("extra_quali", x = quali)
    col_quanti <- update_selectize_colnames("extra_quanti", x = quanti)

    ## Extra variables -----
    extra_quali <- select_data(quali, col_quali, drop = TRUE)
    extra_quanti <- select_data(quanti, col_quanti, drop = TRUE)

    ## Check data -----
    old <- reactive({ x() }) |> bindEvent(input$go)
    notify_change(session$ns("change"), x, old, title = tr_("Beta Diversity"))

    ## Compute similarity -----
    compute_beta <- ExtendedTask$new(
      function(x, method) {
        mirai::mirai({ 1 - tabula::similarity(x, method) }, environment())
      }
    ) |>
      bslib::bind_task_button("go")

    observe({
      compute_beta$invoke(x = x(), method = input$method)
    }) |>
      bindEvent(input$go)

    results <- reactive({
      notify(compute_beta$result(), title = tr_("Beta Diversity"))
    })

    ## Compute PCoA -----
    analysis <- reactive({
      req(results())
      validate_na(results())
      notify(dimensio::pcoa(results(), rank = 2))
    })

    ## Graphical parameters -----
    param_diss <- graphics_server("par_diss")
    param_pcoa <- graphics_server("par_pcoa")

    ## Plot -----
    plot_diss <- reactive({
      req(results())

      function() {
        tabula::plot_heatmap(
          object = results(),
          color = param_diss$pal_quant,
          diag = FALSE,
          upper = FALSE,
          fixed_ratio = TRUE
        )
      }
    })

    plot_pcoa <- reactive({
      req(analysis())

      if (!isTruthy(extra_quali()) && isTruthy(extra_quanti())) {
        col <- param_pcoa$pal_quanti
      } else {
        col <- param_pcoa$pal_quali
      }

      function() {
        dimensio::plot(
          x = analysis(),
          labels = input$pcoa_labels,
          extra_quali = extra_quali(),
          extra_quanti = extra_quanti(),
          color = col,
          symbol = param_pcoa$pal_pch,
          size = param_pcoa$pal_cex,
          panel.first = graphics::grid()
        )

        if (isTRUE(input$hull)) {
          dimensio::viz_hull(
            x = analysis(),
            group = extra_quali(),
            color = param_pcoa$pal_quali
          )
        }
      }
    })

    ## Render plot -----
    render_plot("plot_diss", x = plot_diss)
    render_plot("plot_pcoa", x = plot_pcoa)

    ## Download -----
    output$download_beta <- export_table(results, "beta")
    output$download_pcoa <- downloadHandler(
      filename = function() { make_file_name("pcoa", "zip") },
      content = function(file) {
        dimensio::export(analysis(), file = file, flags = "-r9Xj")
      },
      contentType = "application/zip"
    )
  })
}
