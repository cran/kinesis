# UI ===========================================================================
#' Prepare Data UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [prepare_server()]
#' @family generic modules
#' @keywords internal
#' @export
prepare_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Data"),
    value = "data",
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Data"),
        import_ui(ns("import")),
        select_ui(ns("select")),
        clean_ui(ns("clean"))
      ), # sidebar
      ## Output: value box
      box_ui(ns("box")),
      navset_card_pill(
        placement = "above",
        nav_panel(
          title = tr_("Data"),
          checkboxInput(
            inputId = ns("head"),
            label = tr_("Table overview"),
            value = TRUE),
          gt::gt_output(outputId = ns("table"))
        ),
        nav_panel(
          title = tr_("Missing values"),
          missing_ui(ns("missing"))
        )
      ),
      border_radius = FALSE,
      fillable = TRUE,
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Prepare Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param choose A predicate [`function`] used to select columns.
#' @param select A predicate [`function`] used to select columns.
#' @param demo A [`character`] string specifying the name of a dataset (see
#'  [import_server()]).
#' @return A reactive [`data.frame`].
#' @seealso [prepare_ui()]
#' @family generic modules
#' @keywords internal
#' @export
prepare_server <- function(id, choose = \(...) TRUE, select = \(...) TRUE,
                           demo = NULL) {
  moduleServer(id, function(input, output, session) {
    ## Prepare data -----
    data_clean <- import_server("import", demo = demo) |>
      select_server("select", x = _, find_col = choose, use_col = select) |>
      clean_server("clean", x = _) |>
      missing_server("missing", x = _)

    ## Render description -----
    box_server("box", x = data_clean)

    ## Render table -----
    output$table <- gt::render_gt({
      tbl <- if (isTRUE(input$head)) utils::head(data_clean()) else data_clean()
      gt::gt(tbl, rownames_to_stub = TRUE) |>
        gt::tab_options(table.width = "100%")
    })

    data_clean
  })
}

# Modules ======================================================================
## Value box -------------------------------------------------------------------
box_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = breakpoints(
      xs = c(12, 12, 12, 12),
      md = c(6, 6, 6, 6),
      lg = c(3, 3, 3, 3)
    ),
    fill = FALSE,
    value_box(
      title = tr_("Dimensions"),
      value = textOutput(outputId = ns("value_dimensions"))
    ),
    value_box(
      title = tr_("Sparsity"),
      value = textOutput(outputId = ns("value_sparsity"))
    ),
    value_box(
      title = tr_("Missing values"),
      value = textOutput(outputId = ns("value_missing"))
    ),
    card(
      helpText(tr_("Export your data for futur use.")),
      downloadButton(
        outputId = ns("download"),
        label = tr_("Download")
      )
    )
  )
}
box_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    output$value_dimensions <- renderText({
      req(x())
      paste0(dim(x()), collapse = " x ")
    })
    output$value_sparsity <- renderText({
      req(x())
      paste0(round(arkhe::sparsity(x()) * 100, 2), "%")
    })
    output$value_missing <- renderText({
      req(x())
      sum(is.na(x()))
    })

    ## Download -----
    output$download <- export_table(x, "data")
  })
}

## Select ----------------------------------------------------------------------
select_ui <- function(id) {
  ns <- NS(id)

  tags$div(
    h5(tr_("Select data")),
    selectize_ui(id = ns("rownames"), label = tr_("Sample names")),
    selectize_ui(id = ns("colnames"), label = tr_("Variables"), multiple = TRUE)
  )
}

#' @param id A [`character`] string specifying the namespace.
#' @param x A reactive `matrix`-like object.
#' @param find_col A predicate [`function`] for column detection
#'  (see [arkhe::detect()]).
#' @param use_col A predicate [`function`] for column selection
#'  (see [arkhe::detect()]).
#' @param min_row An [`interger`] specifying the expected minimum number of rows.
#' @param min_col An [`interger`] specifying the expected minimum number of columns.
#' @return A reactive [`data.frame`].
#' @noRd
select_server <- function(id, x, find_col = NULL, use_col = NULL,
                          min_row = 1, min_col = 1) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI
    row_names <- update_selectize_variables(id = "rownames", x = x)

    ## Assign row names
    named <- reactive({
      req(x())
      out <- notify(
        {
          column <- arkhe::seek_columns(x(), names = row_names())
          arkhe::assign_rownames(x(), column = column %|||% 0, remove = TRUE)
        },
        title = tr_("Row names")
      )
      out
    }) |>
      bindEvent(row_names())

    ## Update UI
    col_names <- update_selectize_variables(
      id = "colnames",
      x = named,
      find = find_col,
      use = use_col
    )

    ## Select variables
    selected <- reactive({
      out <- arkhe::get_columns(named(), names = col_names())
      validate_dim(out, i = min_row, j = min_col)
      out
    }) |>
      bindEvent(col_names(), ignoreNULL = FALSE) |>
      debounce(500)

    selected
  })
}

## Clean -----------------------------------------------------------------------
clean_ui <- function(id) {
  ns <- NS(id)

  tags$div(
    h5(tr_("Clean values")),
    ## Input: remove whitespace
    checkboxInput(
      inputId = ns("remove_whitespace"),
      label = tr_("Remove leading/trailing whitespace"),
      value = FALSE
    ),
    ## Input: remove zero
    checkboxInput(
      inputId = ns("remove_zero_row"),
      label = tr_("Remove rows with zero"),
      value = FALSE
    ),
    checkboxInput(
      inputId = ns("remove_zero_column"),
      label = tr_("Remove columns with zero"),
      value = FALSE
    ),
    ## Input: remove constant
    checkboxInput(
      inputId = ns("remove_constant_column"),
      label = tr_("Remove constant columns"),
      value = FALSE
    ),
    ## Input: remove all?
    checkboxInput(
      inputId = ns("all"),
      label = tr_("Remove only if all values meet the condition"),
      value = TRUE,
      width = "100%"
    )
  )
}

#' @param id A [`character`] string specifying the namespace.
#' @param x A reactive `matrix`-like object.
#' @param verbose A [`logical`] scalar: should \R report extra information on
#'  progress?
#' @return A reactive [`data.frame`].
#' @noRd
clean_server <- function(id, x, verbose = get_option("verbose", FALSE)) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    reactive({
      out <- x()

      ## Clean whitespace
      if (isTruthy(out) && isTRUE(input$remove_whitespace)) {
        out <- arkhe::clean_whitespace(out, squish = TRUE)
      }

      ## Remove rows
      ## If only zeros
      if (isTruthy(out) && isTRUE(input$remove_zero_row)) {
        out <- arkhe::remove_zero(out, margin = 1, all = input$all,
                                  verbose = verbose)
      }

      ## Remove columns
      ## If only zeros
      if (isTruthy(out) && isTRUE(input$remove_zero_column)) {
        out <- arkhe::remove_zero(out, margin = 2, all = input$all,
                                  verbose = verbose)
      }
      ## If constant
      if (isTruthy(out) && isTRUE(input$remove_constant_column)) {
        out <- arkhe::remove_constant(out, verbose = verbose)
      }

      validate_dim(out)

      out
    })
  })
}

## Missing ---------------------------------------------------------------------
missing_ui <- function(id) {
  ns <- NS(id)

  layout_column_wrap(
    width = 1/2,
    list(
      ## Input: empty as missing
      checkboxInput(
        inputId = ns("empty_as_NA"),
        label = tr_("Empty string as missing value"),
        value = FALSE
      ),
      ## Input: zero as missing
      checkboxInput(
        inputId = ns("zero_as_NA"),
        label = tr_("Zero as missing value"),
        value = FALSE
      ),
      ## Input: remove missing
      radioButtons(
        inputId = ns("remove"),
        label = tr_("Remove missing values:"),
        choiceNames = c(
          tr_("Keep as is"),
          tr_("Replace missing values with zeros"),
          tr_("Remove rows with missing values"),
          tr_("Remove columns with missing values")
        ),
        choiceValues = c("none", "zero", "row", "col")
      )
    ),
    output_plot(ns("heatmap"))
  )
}

#' @param id A [`character`] string specifying the namespace.
#' @param x A reactive `matrix`-like object.
#' @param verbose A [`logical`] scalar: should \R report extra information on
#'  progress?
#' @return A reactive [`data.frame`].
#' @noRd
missing_server <- function(id, x, verbose = get_option("verbose", FALSE)) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    data_replace <- reactive({
      out <- x()

      ## Replace empty strings
      if (isTRUE(input$empty_as_NA)) {
        out <- arkhe::replace_empty(out, value = NA)
      }

      ## Replace zeros
      if (isTRUE(input$zero_as_NA)) {
        out <- arkhe::replace_zero(out, value = NA)
      }

      out
    })

    data_missing <- reactive({
      out <- data_replace()

      ## Remove missing values
      choice <- input$remove %|||% ""
      fun <- switch(
        choice,
        zero = function(x) {
          arkhe::replace_NA(x, value = 0)
        },
        row = function(x) {
          arkhe::remove_NA(x, margin = 1, all = FALSE, verbose = verbose)
        },
        col = function(x) {
          arkhe::remove_NA(x, margin = 2, all = FALSE, verbose = verbose)
        },
        function(x) { x }
      )
      out <- fun(out)

      validate_dim(out)

      out
    })

    ## Render plot
    plot_missing <- reactive({
      req(data_missing())
      function() {
        col <- if (anyNA(data_missing())) c("#DDDDDD", "#BB5566") else "#DDDDDD"
        tabula::plot_heatmap(object = is.na(data_missing()), color = col,
                             fixed_ratio = FALSE)
      }
    })
    render_plot("heatmap", x = plot_missing)

    data_missing
  })
}
