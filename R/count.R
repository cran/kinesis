# UI ===========================================================================
#' Count Data UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [count_server()]
#' @family count data modules
#' @keywords internal
#' @export
count_ui <- function(id) {
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
          layout_sidebar(
            sidebar = sidebar(
              checkbox_ui(
                id = ns("parts"),
                label = tooltip(
                  trigger = span(
                    tr_("Count data"),
                    icon("info-circle")
                  ),
                  tr_("Select the variables you want to use.")
                )
              )
            ),
            checkboxInput(
              inputId = ns("head"),
              label = tr_("Table overview"),
              value = TRUE
            ),
            gt::gt_output(outputId = ns("table"))
          ) # sidebar
        ), # layout_sidebar
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
#' Count Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param demo A [`character`] string specifying the name of a dataset (see
#'  [import_server()]).
#' @return A reactive [`data.frame`].
#' @seealso [count_ui()]
#' @family count data modules
#' @keywords internal
#' @export
count_server <- function(id, demo = NULL) {
  moduleServer(id, function(input, output, session) {
    ## Prepare data -----
    data_raw <- import_server("import", demo = demo)
    quanti <- subset_quantitative(data_raw, positive = TRUE)

    ## Update UI -----
    parts <- update_checkbox_colnames("parts", x = quanti)

    ## Select variables -----
    data_count <- select_data(quanti, names = parts, drop = FALSE) |>
      debounce(500)

    ## Clean data -----
    data_clean <- clean_server("clean", x = data_count)
    data_missing <- missing_server("missing", x = data_clean)

    ## Check -----
    data_valid <- reactive({
      validate_csv(data_missing())
      validate_dim(data_missing(), i = 1, j = 1)
      validate_na(data_missing())

      data_missing()
    })

    ## Render description -----
    box_server("box", x = data_valid)

    ## Render table -----
    output$table <- gt::render_gt({
      req(data_missing())
      tbl <- if (isTRUE(input$head)) utils::head(data_missing()) else data_missing()
      gt::gt(tbl, rownames_to_stub = TRUE) |>
        gt::tab_options(table.width = "100%")
    })

    data_valid
  })
}
