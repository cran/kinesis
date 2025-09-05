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
        select_calendar(ns("calendar")),
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
    quanti <- subset_quantitative(x)
    quali <- subset_qualitative(x)
    col_lower <- update_selectize_colnames("lower", x = quanti)
    col_upper <- update_selectize_colnames("upper", x = quanti, exclude = col_lower)
    col_groups <- update_selectize_colnames("groups", x = quali)

    lower <- select_data(x, col_lower, drop = TRUE)
    upper <- select_data(x, col_upper, drop = TRUE)
    groups <- select_data(x, col_groups, drop = TRUE)

    calendar <- get_calendar("calendar")

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

      if (isTruthy(groups())) {
        grp <- groups()
        col <- param$col_quali(grp)
      }

      function() {
        aion::plot(results(), calendar = aion::CE(), groups = grp, col = col)
        if (isTruthy(groups())) {
          graphics::legend(x = "topleft", legend = unique(grp), fill = unique(col))
        }
      }
    })

    ## Render plots -----
    render_plot("plot", x = plot)

    reactive({ list(results = results(), groups = groups()) })
  })
}
