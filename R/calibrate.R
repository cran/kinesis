# UI ===========================================================================
#' Radiocarbon Calibration UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [calibrate_server()]
#' @family chronology modules
#' @keywords internal
#' @export
calibrate_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Calibration"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Calibration"),
        ## Input: checkbox if F14C calibration
        checkboxInput(
          inputId = ns("f14c"),
          label = bslib::tooltip(
            trigger = list(HTML("F<sup>14</sup>C"), icon("info-circle")),
            HTML(tr_("Should the calibration be carried out in F<sup>14</sup>C space?"),
                 tr_("If so, your data must be expressed as F<sup>14</sup>C."))
          ),
          value = FALSE
        ),
        accordion(
          accordion_panel(
            title = tr_("Data"),
            ## Input: select variables
            helpText(
              tr_("Select the corresponding columns from your data set."),
              tr_("Leave blank to use the default parameters.")
            ),
            selectize_ui(id = ns("multi_names"), label = tr_("Laboratory codes")),
            selectize_ui(id = ns("multi_values"), label = tr_("Values"),
                         help = tr_("BP ages or F<sup>14</sup>C values to be calibrated.")),
            selectize_ui(id = ns("multi_errors"), label = tr_("Errors"),
                         help = tr_("Errors associated to the values to be calibrated.")),
            selectize_ui(id = ns("multi_marine_offsets"), label = tr_("Reservoir offsets"),
                         help = tr_("Offset values for any marine reservoir effect.")),
            selectize_ui(id = ns("multi_marine_errors"), label = tr_("Reservoir errors"),
                         help = tr_("Offset value errors for any marine reservoir effect.")),
            selectize_ui(id = ns("multi_curves"), label = tr_("Curves"),
                         help = tr_("Calibration curves to be used.")),
            selectize_ui(id = ns("multi_positions"), label = tr_("Positions"),
                         help = tr_("Position values (e.g. depths) for each age."))
          ),
          accordion_panel(
            title = tr_("Default parameters"),
            radioButtons(
              inputId = ns("calib_curve"),
              label = tr_("Calibration curve:"),
              choices = c("IntCal20", "Marine20", "SHCal20"),
              selected = "IntCal20"
            ),
            numericInput(
              inputId = ns("calib_marine_offset"),
              label = tr_("Reservoir offset"),
              value = 0
            ),
            numericInput(
              inputId = ns("calib_marine_error"),
              label = tr_("Reservoir error"),
              value = 0
            ),
            numericInput(
              inputId = ns("calib_from"),
              label = bslib::tooltip(
                trigger = list(tr_("From"), icon("info-circle")),
                tr_("Earliest date to calibrate for, in cal. BP years.")
              ),
              value = 55000
            ),
            numericInput(
              inputId = ns("calib_to"),
              label = bslib::tooltip(
                trigger = list(tr_("To"), icon("info-circle")),
                tr_("Latest date to calibrate for, in cal. BP years.")
              ),
              value = 0
            ),
            numericInput(
              inputId = ns("calib_resolution"),
              label = bslib::tooltip(
                trigger = list(tr_("Resolution"), icon("info-circle")),
                tr_("Temporal resolution of the calibration, in years.")
              ),
              value = 1
            )
          )
        ),
        bslib::input_task_button(id = ns("go"), label = tr_("(Re)Calibrate")),
        render_export_button(ns("export_results"))
      ), # sidebar
      navset_card_pill(
        sidebar = sidebar(
          title = tr_("Options"),
          select_calendar(ns("calendar")),
          radioButtons(
            inputId = ns("level"),
            label = tr_("Level:"),
            selected = "0.95",
            choiceNames = c("68%", "95%", "99%"),
            choiceValues = c("0.68", "0.95", "0.99")
          )
        ),
        nav_panel(
          title = tr_("Results"),
          gt::gt_output(outputId = ns("table_results"))
        ),
        nav_panel(
          title = tr_("Plot"),
          output_plot(
            id = ns("plot_density"),
            tools = list(
              checkboxInput(inputId = ns("fixed"), value = TRUE, label = tr_("Fixed scale")),
              checkboxInput(inputId = ns("sort_density"), label = tr_("Decreasing order")),
              selectize_ui(
                id = ns("extra_quali"),
                label = tr_("Extra qualitative variable")
              ),
              graphics_ui(
                id = ns("par_dens"),
                col_quant = FALSE,
                pch = FALSE,
                lty = FALSE,
                cex = FALSE
              )
            )
          )
        ),
        nav_panel(
          title = tr_("Intervals"),
          layout_column_wrap(
            output_plot(
              id = ns("plot_intervals"),
              tools = list(
                checkboxInput(inputId = ns("sort_intervals"), label = tr_("Decreasing order")),
                graphics_ui(
                  id = ns("par_int"),
                  col_quali = FALSE,
                  col_quant = FALSE,
                  pch = FALSE,
                  lty = FALSE,
                  size_range = FALSE
                )
              )
            ),
            # render_export_button(ns("export_intervals")),
            tableOutput(outputId = ns("table_intervals"))
          )
        )
      ) # navset_card_pill
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Radiocarbon Calibration Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @return A reactive [`ananke::CalibratedAges-class`] object.
#' @seealso [calibrate_ui()]
#' @family chronology modules
#' @keywords internal
#' @export
calibrate_server  <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI -----
    quali <- subset_qualitative(x)
    quanti <- subset_quantitative(x)

    calendar <- get_calendar("calendar")
    values <- update_selectize_colnames("multi_values", x = quanti)
    errors <- update_selectize_colnames("multi_errors", x = quanti)
    curves <- update_selectize_colnames("multi_curves", x = quali)
    names <- update_selectize_colnames("multi_names", x = quali)
    positions <- update_selectize_colnames("multi_positions", x = quanti)
    res_off <- update_selectize_colnames("multi_marine_offsets", x = quanti)
    res_err <- update_selectize_colnames("multi_marine_errors", x = quanti)

    col_quali <- update_selectize_colnames("extra_quali", x = quali)
    extra_quali <- select_data(quali, col_quali, drop = TRUE)

    ## Calibrate -----
    compute_calib <- ExtendedTask$new(
      function(x, ...) {
        mirai::mirai({
          param <- list(...)
          do.call(ananke::c14_calibrate, param)
        }, environment())
      }
    ) |>
      bslib::bind_task_button("go")

    observe({
      req(values(), errors())
      compute_calib$invoke(
        values = x()[[values()]],
        errors = x()[[errors()]],
        curves = if (isTruthy(curves())) x()[[curves()]] else input$calib_curve,
        names = if (isTruthy(names())) x()[[names()]] else NULL,
        positions = if (isTruthy(positions())) x()[[positions()]] else NULL,
        reservoir_offsets = if (isTruthy(res_off())) x()[[res_off()]] else input$calib_marine_offset %|||% 0,
        reservoir_errors = if (isTruthy(res_err())) x()[[res_err()]] else input$calib_marine_error %|||% 0,
        from = input$calib_from %|||% 55000,
        to = input$calib_to %|||% 0,
        resolution = input$calib_resolution %|||% 1
      )
    }) |>
      bindEvent(input$go)

    old <- reactive({ x() }) |> bindEvent(input$go)
    results <- reactive({
      if (!identical(x(), old())) return(NULL) # Invalidate
      notify(compute_calib$result(), title = tr_("Radiocarbon Calibration"))
    })

    ## Compute intervals -----
    intervals <- reactive({
      req(results())
      ananke::interval_hdr(x = results(), level = as.numeric(input$level))
    })

    ## Render table -----
    data_calib <- reactive({
      req(results())
      df <- ananke::as.data.frame(
        x = results(),
        level = as.numeric(input$level),
        calendar = calendar()
      )
      df[, -ncol(df)]
    })
    data_intervals <- reactive({
      req(intervals())
      ananke::as.data.frame(intervals(), calendar = calendar())
    })
    output$table_results <- gt::render_gt({
      gt::gt(data_calib(), rownames_to_stub = TRUE) |>
        gt::tab_options(table.width = "100%") |>
        gt::tab_spanner(
          label = tr_("Conventional Age"),
          columns = gt::starts_with("BP14C")
        ) |>
        gt::tab_spanner(
          label = "F14C",
          columns = gt::starts_with("F14C")
        ) |>
        gt::tab_spanner(
          label = tr_("Marine Reservoir"),
          columns = gt::starts_with("reservoir")
        ) |>
        gt::tab_spanner(
          label = tr_("Calibration"),
          columns = gt::starts_with("calibration")
        ) |>
        gt::cols_label(
          BP14C_value = tr_("Value"),
          BP14C_error = tr_("Error"),
          reservoir_offset = tr_("Offset"),
          reservoir_error = tr_("Error"),
          calibration_curve = tr_("Curve"),
          calibration_hdr = tr_("HDR")
        ) |>
        gt::opt_interactive(
          use_compact_mode = TRUE,
          use_page_size_select = TRUE
        )
    })
    output$table_intervals <- gt::render_gt({
      gt::gt(data_intervals(), rowname_col = "label") |>
        gt::tab_options(table.width = "100%") |>
        gt::cols_label(
          start = tr_("From"),
          end = tr_("To"),
          p = tr_("p")
        ) |>
        gt::opt_interactive(
          use_compact_mode = TRUE,
          use_page_size_select = TRUE
        )
    })

    ## Graphical parameters -----
    param_dens <- graphics_server("par_dens")
    param_int <- graphics_server("par_int")

    ## Render plot -----
    plot_density <- reactive({
      req(results())

      col <- "grey"
      if (isTruthy(extra_quali())) {
        col <- param_dens$col_quali(extra_quali())
      }

      function() {
        ananke::ridgelines(
          x = results(),
          calendar = calendar(),
          interval = "hdr",
          level = as.numeric(input$level),
          fixed = isTRUE(input$fixed),
          decreasing = isTRUE(input$sort_density),
          col = col
        )

        if (isTruthy(extra_quali())) {
          graphics::legend(
            x = ifelse(isTRUE(input$sort_density), "topright", "topleft"),
            legend = unique(extra_quali()),
            fill = unique(col),
            bty = "n"
          )
        }
      }
    })
    plot_intervals <- reactive({
      req(intervals())
      function() {
        aion::plot(
          x = intervals(),
          calendar = calendar(),
          decreasing = isTRUE(input$sort_intervals),
          lwd = param_int$pal_cex
        )
      }
    })
    render_plot("plot_density", x = plot_density)
    render_plot("plot_intervals", x = plot_intervals)

    ## Download -----
    export_table("export_results", data_calib, name = "calibration")
    # export_table("export_interval", data_intervals, name = "intervals")

    results
  })
}
