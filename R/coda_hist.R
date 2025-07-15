# UI ===========================================================================
#' Compositional Histogram UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [coda_hist_server()]
#' @family coda modules
#' @keywords internal
#' @export
coda_hist_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Histogram"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Histogram"),
        selectize_ui(id = ns("select"), label = tr_("Select a part"))
      ), # sidebar
      output_plot(
        id = ns("hist"),
        title = tr_("Histogram"),
        note = info_article(author = "Filzmoser et al.", year = "2009",
                            doi = "10.1016/j.scitotenv.2009.08.008")
      )
    ) # layout_sidebar
  ) # nav_panel
}


# Server =======================================================================
#' Compositional Histogram Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`nexus::CompositionMatrix-class`] object.
#' @return
#'  No return value, called for side effects.
#' @seealso [coda_hist_ui()]
#' @family coda modules
#' @keywords internal
#' @export
coda_hist_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Select column -----
    col_hist <- update_selectize_variables("select", x = x, preserve = FALSE, none = FALSE)

    ## Histogram -----
    plot_hist <- reactive({
      req(x(), col_hist())
      function() nexus::hist(x(), select = col_hist())
    })

    ## Render histogram -----
    render_plot("hist", x = plot_hist)
  })
}
