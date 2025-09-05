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
        import_ui(ns("import"))
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
          title = tr_("Clean values"),
          clean_ui(ns("clean"))
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
#' @param demo A [`character`] string specifying the name of a dataset (see
#'  [import_server()]).
#' @return A reactive [`data.frame`].
#' @seealso [prepare_ui()]
#' @family generic modules
#' @keywords internal
#' @export
prepare_server <- function(id, demo = NULL) {
  moduleServer(id, function(input, output, session) {
    ## Prepare data -----
    data_raw <- import_server("import", demo = demo)
    data_clean <- clean_server("clean", x = data_raw)
    data_miss <- missing_server("missing", x = data_clean)

    ## Render description -----
    box_server("box", x = data_miss)

    ## Render table -----
    output$table <- gt::render_gt({
      tbl <- if (isTRUE(input$head)) utils::head(data_miss()) else data_miss()
      gt::gt(tbl, rownames_to_stub = TRUE) |>
        gt::tab_options(table.width = "100%")
    })

    data_miss
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
      uiOutput(outputId = ns("value_dimensions"))
    ),
    value_box(
      title = tr_("Sparsity"),
      uiOutput(outputId = ns("value_sparsity"))
    ),
    value_box(
      title = tr_("Missing values"),
      uiOutput(outputId = ns("value_missing"))
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
    output$value_dimensions <- renderUI({
      req(x())
      paste0(dim(x()), collapse = " x ")
    })
    output$value_sparsity <- renderUI({
      req(x())
      paste0(round(arkhe::sparsity(x()) * 100, 2), "%")
    })
    output$value_missing <- renderUI({
      req(x())
      sum(is.na(x()))
    })

    ## Download -----
    output$download <- export_table(x, "data")
  })
}

## Clean -----------------------------------------------------------------------
clean_ui <- function(id) {
  ns <- NS(id)

  list(
    ## Input: remove whitespace
    checkboxInput(
      inputId = ns("remove_whitespace"),
      label = tr_("Remove leading/trailing whitespace"),
      value = FALSE,
      width = "100%"
    ),
    ## Input: remove zero
    checkboxInput(
      inputId = ns("remove_zero_row"),
      label = tr_("Remove rows with zero"),
      value = FALSE,
      width = "100%"
    ),
    checkboxInput(
      inputId = ns("remove_zero_column"),
      label = tr_("Remove columns with zero"),
      value = FALSE,
      width = "100%"
    ),
    ## Input: remove constant
    checkboxInput(
      inputId = ns("remove_constant_column"),
      label = tr_("Remove constant columns"),
      value = FALSE,
      width = "100%"
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
    ## Clean whitespace
    no_ws <- reactive({
      req(x())
      if (!isTRUE(input$remove_whitespace)) return(x())
      arkhe::clean_whitespace(x(), squish = TRUE)
    })

    ## Remove (all) rows with zeros
    no_zero_row <- reactive({
      req(no_ws())
      if (!isTRUE(input$remove_zero_row)) return(no_ws())
      arkhe::remove_zero(no_ws(), margin = 1, all = isTRUE(input$all),
                         verbose = verbose)
    })

    ## Remove (all) columns with zeros
    no_zero_col <- reactive({
      req(no_zero_row())
      if (!isTRUE(input$remove_zero_column)) return(no_zero_row())
      arkhe::remove_zero(no_zero_row(), margin = 2, all = isTRUE(input$all),
                         verbose = verbose)
    })

    ## Remove constant columns
    no_cte <- reactive({
      req(no_zero_col())
      if (!isTRUE(input$remove_constant_column)) return(no_zero_col())
      arkhe::remove_constant(no_zero_col(), verbose = verbose)
    })

    no_cte
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
    ## Replace empty strings
    empty_as_na <- reactive({
      req(x())
      if (!isTRUE(input$empty_as_NA)) return(x())
      arkhe::replace_empty(x(), value = NA)
    })

    ## Replace zeros
    zero_as_na <- reactive({
      req(empty_as_na())
      if (!isTRUE(input$zero_as_NA)) return(empty_as_na())
      arkhe::replace_zero(empty_as_na(), value = NA)
    })

    ## Remove missing values
    no_missing <- reactive({
      req(zero_as_na())

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

      fun(zero_as_na())
    })

    ## Render plot
    plot_missing <- reactive({
      req(all(dim(no_missing()) > 0))
      function() {
        col <- if (anyNA(no_missing())) c("#DDDDDD", "#BB5566") else "#DDDDDD"
        tabula::plot_heatmap(object = is.na(no_missing()), color = col,
                             fixed_ratio = FALSE)
      }
    })
    render_plot("heatmap", x = plot_missing)

    no_missing
  })
}
