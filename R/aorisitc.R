# UI ===========================================================================
#' Aoristic Analysis UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [aoristic_server()]
#' @family chronology modules
#' @keywords internal
#' @export
aoristic_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Analysis"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Aoristic Analysis"),
        numericInput(
          inputId = ns("step"),
          label = tr_("Temporal step size"),
          value = 10,
          min = 1,
          max = 500
        ),
        numericInput(
          inputId = ns("start"),
          label = tr_("Beginning of the time window"),
          value = NULL
        ),
        numericInput(
          inputId = ns("end"),
          label = tr_("End of the time window"),
          value = NULL
        ),
        select_calendar(ns("calendar")),
        checkboxInput(
          inputId = ns("weight"),
          label = tr_("Weigth"),
          value = FALSE
        ),
        info_article("Ratcliffe", "2002", "10.1023/A:1013240828824"),
        bslib::input_task_button(id = ns("go"), label = tr_("(Re)Compute")),
        downloadButton(
          outputId = ns("download"),
          label = tr_("Download results")
        )
      ), # sidebar
      output_plot(id = ns("plot_ao"))
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Aoristic Analysis Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `list` (returned by [time_interval_server()])
#' @return A reactive [`kairos::AoristicSum-class`] object.
#' @seealso [aoristic_ui()]
#' @family chronology modules
#' @keywords internal
#' @export
aoristic_server <- function(id, x, y) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(y))

  moduleServer(id, function(input, output, session) {
    ## Update UI -----
    calendar <- get_calendar("calendar")
    lower <- reactive({
      req(x()$results)
      aion::start(x()$results, calendar = calendar())
    })
    upper <- reactive({
      req(x()$results)
      aion::end(x()$results, calendar = calendar())
    })

    observe({
      updateNumericInput(inputId = "start", value = min(lower(), na.rm = TRUE))
    }) |>
      bindEvent(lower())
    observe({
      updateNumericInput(inputId = "end", value = max(upper(), na.rm = TRUE))
    }) |>
      bindEvent(upper())


    ## Check data -----
    old <- reactive({ x() }) |> bindEvent(input$go)
    notify_change(session$ns("change"), x, old, title = tr_("Aoristic Analysis"))

    ## Compute analysis -----
    compute_ao <- ExtendedTask$new(
      function(x, y, step, start, end, calendar, weight, groups) {
        mirai::mirai({
          kairos::aoristic(x, y, step, start, end, calendar, weight, groups)
        }, environment())
      }
    ) |>
      bslib::bind_task_button("go")

    observe({
      compute_ao$invoke(x = lower(), y = upper(), step = input$step,
                        start = input$start, end = input$end,
                        calendar = calendar(), weight = input$weight,
                        groups = x()$groups)
    }) |>
      bindEvent(input$go)

    results <- reactive({
      notify(compute_ao$result(), title = tr_("Aoristic Analysis"))
    })

    ## Plot -----
    plot_ao <- reactive({
      req(results())
      function() {
        kairos::plot(results(), calendar = calendar(), col = "grey")
      }
    })

    ## Render plots -----
    render_plot("plot_ao", x = plot_ao)

    ## Export -----
    output$download <- export_table(results, "aoristic")

    results
  })
}
