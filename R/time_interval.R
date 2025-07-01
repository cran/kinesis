# UI ===========================================================================
#' Time Intervals UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [time_interval_server()]
#' @family chronology modules
#' @keywords internal
#' @export
time_interval_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Intervals"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Time Intervals"),
        selectize_ui(
          id = ns("lower"),
          label = tr_("Lower temporal boundary")
        ),
        selectize_ui(
          id = ns("upper"),
          label = tr_("Upper temporal boundary")
        ),
        selectizeInput(
          inputId = ns("calendar"),
          label = tr_("Calendar"),
          choices = c("CE", "BCE", "BP"),
          selected = "CE"
        ),
        selectize_ui(
          id = ns("groups"),
          label = tr_("Groups")
        )
      ), # sidebar
      output_plot(
        id = ns("plot"),
        tools = graphics_ui(ns("par"), col_quant = FALSE, pch = FALSE,
                            lty = FALSE, cex = FALSE)
      )
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Aoristic Analysis Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame`.
#' @return A reactive [`list`].
#' @seealso [time_interval_ui()]
#' @family chronology modules
#' @keywords internal
#' @export
time_interval_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI -----
    col_lower <- update_selectize_variables("lower", x = x, find = is.numeric)
    col_upper <- update_selectize_variables("upper", x = x, find = is.numeric)
    col_groups <- update_selectize_variables("groups", x = x, find = Negate(is.numeric))

    lower <- reactive({
      req(col_lower())
      x()[[col_lower()]]
    })
    upper <- reactive({
      req(col_upper())
      x()[[col_upper()]]
    })
    groups <- reactive({
      if (isTruthy(col_groups())) x()[[col_groups()]] else NULL
    })
    calendar <- reactive({
      req(input$calendar)
      aion::calendar(input$calendar)
    })

    ## Time Intervals -----
    results <- reactive({
      req(x(), lower(), upper(), calendar())
      notify(
        {
          aion::intervals(start = lower(), end = upper(),
                          calendar = calendar(), names = rownames(x()))
        },
        title = tr_("Aoristic Analysis")
      )
    })

    ## Graphical parameters -----
    param <- graphics_server("par")

    ## Plot -----
    plot <- reactive({
      req(results())
      grp <- NULL
      col <- "black"
      if (length(groups()) > 0) {
        grp <- groups()
        col <- param$col_quali(grp)
      }
      function() {
        aion::plot(results(), calendar = aion::CE(), groups = grp, col = col)
        if (length(groups()) > 0) {
          graphics::legend(x = "topleft", legend = unique(grp),
                           fill = unique(col))
        }
      }
    })

    ## Render plots -----
    render_plot("plot", x = plot)

    reactive({ list(results = results(), groups = groups()) })
  })
}
