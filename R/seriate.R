# UI ===========================================================================
#' CA Seriation UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [seriate_server()]
#' @family chronology modules
#' @keywords internal
#' @export
seriate_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Seriation"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Permutation"),
        ## Input: checkbox if permute rows
        checkboxInput(
          inputId = ns("margin_row"),
          label = tr_("Permute rows"),
          value = TRUE
        ),
        ## Input: checkbox if permute columns
        checkboxInput(
          inputId = ns("margin_col"),
          label = tr_("Permute columns"),
          value = TRUE
        ),
        ## Input: select CA axes
        numericInput(
          inputId = ns("axes"),
          label = tr_("CA dimension"),
          value = 1,
          min = 1,
          max = 10,
          step = 1
        ),
        ## Output: download
        downloadButton(
          outputId = ns("export_table"),
          label = tr_("Export matrix")
        )
      ), # sidebar
      ## Output: plot reordered matrix
      navset_card_pill(
        bertin_ui(
          id = ns("plot"),
          title = tr_("Rearranged matrix")
        )
      )
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' CA Seriation Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @param order A reactive \R object (coercible by [kairos::as_seriation()]).
#' @return A reactive [`kairos::AveragePermutationOrder-class`] object.
#' @seealso [seriate_ui()]
#' @family chronology modules
#' @keywords internal
#' @export
seriate_server  <- function(id, x, order) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(order))

  moduleServer(id, function(input, output, session) {
    ## Seriate -----
    data_seriate <- reactive({
      validate(need(order(), tr_("Compute the seriation order first.")))

      margin <- NULL
      if (input$margin_row) margin <- c(margin, 1)
      if (input$margin_col) margin <- c(margin, 2)

      kairos::as_seriation(order(), margin = margin, axes = input$axes)
    })

    ## Permute -----
    data_permute <- reactive({
      req(x())
      req(data_seriate())
      kairos::permute(x(), data_seriate())
    })

    ## Render plot -----
    bertin_server("plot", x = data_permute)

    ## Download -----
    output$export_table <- export_table(data_permute, name = "permuted")

    data_seriate
  })
}
