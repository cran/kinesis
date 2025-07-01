# UI ===========================================================================
#' Compositional Data Outliers UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [coda_outliers_server()]
#' @family coda modules
#' @keywords internal
#' @export
coda_outliers_ui <- function(id) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Outliers"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        h5(tr_("Outliers Detection")),
        helpText(
          tr_("See"),
          cite_article("Filzmoser & Hron", "2008", "10.1007/s11004-007-9141-5", after = ";"),
          cite_article("Filzmoser, Hron & Reimann", "2012", "10.1016/j.cageo.2011.06.014", after = ".")
        ),
        radioButtons(
          inputId = ns("method"),
          label = tr_("Multivariate location estimation"),
          choiceNames = c(tr_("Minimum volume ellipsoid"),
                          tr_("Minimum covariance determinant")),
          choiceValues = c("mve", "mcd")
        ),
        sliderInput(
          inputId = ns("quantile"),
          label = tr_("Quantile"),
          min = 0.025, max = 0.995,
          value = 0.975, step = 0.005
        ),
        actionButton(inputId = ns("go"), label = tr_("(Re)Detect")),
        downloadButton(outputId = ns("download"), tr_("Download results"))
      ), # sidebar
      layout_columns(
        col_widths = "50%",
        output_plot(
          id = ns("plot"),
          title = tr_("Plot")
        ),
        div(
          radioButtons(
            inputId = ns("type"),
            label = tr_("Plot type"),
            choices = c("dotchart", "distance")
          ),
          tableOutput(outputId = ns("info"))
        )
      ) # layout_columns
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Compositional Data Outliers Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`nexus::CompositionMatrix-class`] object.
#' @return A reactive [`nexus::OutlierIndex-class`] object.
#' @seealso [coda_outliers_ui()]
#' @family coda modules
#' @keywords internal
#' @export
coda_outliers_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Detect outliers -----
    out <- reactive({
      validate(need(x(), tr_("Check your data.")))

      notify(
        {
          nexus::detect_outlier(
            x(),
            method = input$method,
            quantile = input$quantile
          )
        },
        title = tr_("Outliers Detection")
      )
    }) |>
      bindEvent(input$go)

    ## Plot -----
    plot <- reactive({
      req(out())
      function() {
        plot(out(), type = input$type)
      }
    })
    render_plot("plot", x = plot)

    ## Get data -----
    results <- reactive({
      req(out())
      as.data.frame(out())
    })

    ## Download results -----
    output$download <- export_table(results, name = "coda_outliers")

    out
  })
}
