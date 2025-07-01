# UI ===========================================================================
#' MCD UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [mcd_server()]
#' @family ceramic data modules
#' @keywords internal
#' @export
mcd_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Date"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        h5(tr_("Mean Ceramic Date")),
        helpText(tr_("Set the date midpoint (in years) of each ceramic type.")),
        render_numeric_input(ns("dates")),
        select_calendar(ns("calendar_input")),
        bslib::input_task_button(id = ns("go"), label = tr_("(Re)Compute")),
        downloadButton(
          outputId = ns("download"),
          label = tr_("Download results")
        )
      ), # sidebar
      layout_columns(
        col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6)),
        output_plot(
          id = ns("plot"),
          tools = list(
            select_calendar(ns("calendar_output"))
          )
        ),
        card(
          gt::gt_output(outputId = ns("table"))
        )
      )
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' MCD Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @return
#'  No return value, called for side effects.
#' @seealso [mcd_ui()]
#' @family ceramic data modules
#' @keywords internal
#' @export
mcd_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Check data -----
    old <- reactive({ x() }) |> bindEvent(input$go)
    notify_change(session$ns("change"), x, old, title = tr_("MCD"))

    ## Update UI -----
    dates <- build_numeric_input("dates", x)
    cal_in <- get_calendar("calendar_input")
    cal_out <- get_calendar("calendar_output")

    ## Compute MCD -----
    compute_mcd <- ExtendedTask$new(
      function(x, dates, calendar) {
        mirai::mirai({ kairos::mcd(x, dates, calendar) }, environment())
      }
    ) |>
      bslib::bind_task_button("go")

    observe({
      compute_mcd$invoke(x = x(), dates = dates(), calendar = cal_in())
    }) |>
      bindEvent(input$go)

    results <- reactive({
      notify(compute_mcd$result(), title = tr_("MCD"))
    })

    ## Plot -----
    map <- reactive({
      req(results())
      function() kairos::plot(results(), calendar = cal_out())
    })

    ## Table -----
    tbl <- reactive({
      req(results())
      as.data.frame(results(), calendar = cal_out())
    })

    ## Render table -----
    output$table <- gt::render_gt({
      req(tbl())
      tbl() |>
        gt::gt(rowname_col = "sample") |>
        gt::fmt_number(decimals = 2) |>
        gt::sub_missing()
    })

    ## Render plot -----
    render_plot("plot", x = map)

    ## Download -----
    output$download <- export_table(tbl, "mcd")
  })
}
