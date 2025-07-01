# UI ===========================================================================
#' Correspondence Analysis UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [ca_server()]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
ca_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("CA"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Correspondence Analysis"),
        selectize_ui(
          id = ns("sup_row"),
          label = tr_("Supplementary individuals"),
          multiple = TRUE
        ),
        selectize_ui(
          id = ns("sup_col"),
          label = tr_("Supplementary quantitative variables"),
          multiple = TRUE
        ),
        selectize_ui(
          id = ns("sup_quali"),
          label = tr_("Supplementary qualitative variables"),
          multiple = TRUE
        ),
        bslib::input_task_button(id = ns("go"), label = tr_("(Re)Compute")),
        downloadButton(
          outputId = ns("download"),
          label = tr_("Download results")
        ),
        uiOutput(outputId = ns("chi2"))
      ), # sidebar
      multivariate_ui(ns("ca")),
      border_radius = FALSE,
      fillable = TRUE
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Correspondence Analysis Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame`.
#' @return A reactive [`dimensio::PCA-class`] object.
#' @seealso [ca_ui()]
#' @family multivariate analysis modules
#' @keywords internal
#' @export
ca_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI -----
    row_names <- reactive({ rownames(x()) })
    sup_row <- update_selectize_values("sup_row", x = row_names)
    sup_col <- update_selectize_variables("sup_col", x = x, find = is.numeric)
    sup_quali <- update_selectize_variables("sup_quali", x = x, find = Negate(is.numeric))

    ## Check data -----
    old <- reactive({ x() }) |> bindEvent(input$go)
    notify_change(session$ns("change"), x, old, title = tr_("CA"))

    ## Compute CA -----
    compute_ca <- ExtendedTask$new(
      function(x, rank, sup_row, sup_col, sup_quali) {
        mirai::mirai({
          param <- list(object = x, rank = rank,
                        sup_row = arkhe::seek_rows(x, names = sup_row),
                        sup_col = arkhe::seek_columns(x, names = sup_col))
          if (is.data.frame(x)) {
            param$sup_quali <- arkhe::seek_columns(x, names = sup_quali)
          }
          do.call(dimensio::ca, param)
        }, environment())
      }
    ) |>
      bslib::bind_task_button("go")

    observe({
      compute_ca$invoke(x = x(), rank = input$rank, sup_row = sup_row(),
                        sup_col = sup_col(), sup_quali = sup_quali())
    }) |>
      bindEvent(input$go)

    results <- reactive({
      notify(compute_ca$result(), title = tr_("Correspondence Analysis"))
    })

    multivariate_server("ca", x = results, y = x)

    ## Chi-squared -----
    chi2_test <- reactive({
      req(results())
      x <- dimensio::get_data(results())
      z <- suppressWarnings(stats::chisq.test(x = x))

      # Adjusted Cramer's V [95%CI]
      k <- ncol(x)
      r <- nrow(x)
      n <- sum(x)
      phi <- max(0, (z$statistic / n) - (k - 1) * (r - 1) / (n - 1))

      k_bias <- k - (k - 1)^2 / (n - 1)
      r_bias <- r - (r - 1)^2 / (n - 1)
      V <- sqrt(phi / min(k_bias - 1, r_bias - 1))

      z$cramer <- V
      z
    })

    output$chi2 <- renderUI({
      list(
        h5(tr_("Chi-squared Test")),
        tags$ul(
          tags$li(
            sprintf(tr_("Statistic: %.0f"), chi2_test()$statistic)
          ),
          tags$li(
            sprintf(tr_("Degrees of freedom: %.0f"), chi2_test()$parameter)
          ),
          tags$li(
            sprintf(tr_("p-value: %s"), format.pval(chi2_test()$p.value, eps = .001))
          ),
          tags$li(
            sprintf(tr_("Cramer's V: %.2f"), chi2_test()$cramer)
          )
        )
      )
    })

    ## Export -----
    output$download <- downloadHandler(
      filename = function() { make_file_name("ca", "zip") },
      content = function(file) {
        dimensio::export(results(), file = file, flags = "-r9Xj")
      },
      contentType = "application/zip"
    )

    results
  })
}
