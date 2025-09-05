# UI ===========================================================================
#' Compositional Data UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [coda_server()]
#' @family coda modules
#' @keywords internal
#' @export
coda_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Data"),
    value = "data",
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Compositional Data"),
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
                    tr_("Compositional parts"),
                    icon("info-circle")
                  ),
                  tr_("Select the variables you want to use.")
                )
              ),
              selectize_ui(
                id = ns("group"),
                label = tooltip(
                  trigger = span(
                    tr_("Group by"),
                    icon("info-circle")
                  ),
                  tr_("You can use a qualitative variable to assign each sample to a (reference) group."),
                  tr_("Missing values will be interpreted as unassigned samples.")
                ),
                multiple = TRUE
              ),
              selectize_ui(
                id = ns("condense"),
                label = tooltip(
                  trigger = span(
                    tr_("Condense by"),
                    icon("info-circle")
                  ),
                  tr_("You can use one or more categorical variable to split the data into subsets and compute the compositional mean for each."),
                  tr_("Usefull if your data contain several observations for the same sample (e.g. repeated measurements).")
                ),
                multiple = TRUE
              ),
            ), # sidebar
            ## Output: display data
            checkboxInput(
              inputId = ns("head"),
              label = tr_("Table overview"),
              value = TRUE
            ),
            gt::gt_output(outputId = ns("table"))
          ) # layout_sidebar
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
#' Compositional Data Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param demo A [`character`] string specifying the name of a dataset (see
#'  [import_server()]).
#' @param verbose A [`logical`] scalar: should \R report extra information
#'  on progress?
#' @return A reactive [`nexus::CompositionMatrix-class`] object.
#' @seealso [coda_ui()]
#' @family coda modules
#' @keywords internal
#' @export
coda_server <- function(id, demo = NULL, verbose = get_option("verbose", FALSE)) {
  moduleServer(id, function(input, output, session) {
    ## Prepare data -----
    data_raw <- import_server("import", demo = demo)
    quanti <- subset_quantitative(data_raw, positive = TRUE)
    quali <- subset_qualitative(data_raw)

    ## Update UI -----
    var_parts <- update_checkbox_colnames("parts", x = quanti)
    var_group <- update_selectize_colnames("group", x = quali)
    var_condense <- update_selectize_colnames("condense", x = data_raw)

    ## Compositions -----
    data_coda <- reactive({
      notify(
        nexus::as_composition(
          from = quanti(),
          parts = var_parts(),
          autodetect = FALSE,
          verbose = verbose
        ),
        title = tr_("Compositional Data")
      )
    }) |>
      bindEvent(var_parts()) |>
      debounce(500)

    ## Group -----
    data_group <- reactive({
      if (!isTruthy(var_group()) || !isTruthy(data_coda()))
        return(data_coda())

      nexus::group(data_coda(), by = quali()[var_group()],
                   verbose = verbose)
    })

    ## Condense -----
    data_condense <- reactive({
      if (!isTruthy(var_condense()) || !isTruthy(data_group()))
        return(data_group())

      nexus::condense(data_group(), by = data_raw()[var_condense()],
                      ignore_na = FALSE, verbose = verbose)
    })

    ## Missing values -----
    data_clean <- clean_server("clean", x = data_condense)
    data_missing <- missing_server("missing", x = data_clean)

    ## Zeros -----
    # TODO

    ## Check -----
    data_valid <- reactive({
      validate_csv(data_missing())
      validate_dim(data_missing(), i = 1, j = 3)
      validate_na(data_missing())
      validate_zero(data_missing())

      data_missing()
    })

    ## Render tables -----
    output$table <- gt::render_gt({
      req(data_missing())
      tbl <- as.data.frame(data_missing(), group_var = tr_("Group"))
      tbl <- if (isTRUE(input$head)) utils::head(tbl) else tbl
      gt::gt(tbl, rownames_to_stub = TRUE) |>
        gt::tab_options(table.width = "100%")
    })

    ## Value box -----
    box_server("box", x = data_valid)

    data_valid
  })
}

# Modules ======================================================================
## Imputation ------------------------------------------------------------------
coda_zero_ui <- function(id) {
  ns <- NS(id)

  list(
    helpText(
      tr_("If your data contains zeros, these can be considered as values below the detection limit (i.e. small unknown values)."),
      tr_("In this case, you can define the detection limit for each compositional part below."),
      tr_("If all limits are specified, zeros will be replaced by a fraction of these limits."),
      tr_("For computational details, see"),
      cite_article("Martin-Fernandez et al.", "2003", doi = "10.1023/A:1023866030544", text = TRUE)
    ),
    numericInput(
      inputId = ns("delta"),
      label = tr_("Fraction"),
      value = 2 / 3,
      min = 0,
      max = 1
    ),
    uiOutput(outputId = ns("values")),
    actionButton(inputId = ns("go"), tr_("Replace zero"))
  )
}
coda_zero_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    data <- reactiveValues(values = NULL)

    ## Build UI
    ids <- reactive({
      if (is.null(colnames(x()))) return(NULL)
      data$values <- x()
      paste0("limit_", colnames(x()))
    })

    ui <- reactive({
      req(ids())

      ui <- lapply(
        X = ids(),
        FUN = function(i) {
          numericInput(
            inputId = session$ns(i),
            label = paste(sub("limit_", "", i), "(%)", sep = " "),
            value = 0, min = 0, max = 100
          )
        }
      )

      do.call(layout_column_wrap, args = c(ui, width = 1/4))
    })
    output$values <- renderUI({ ui() })
    outputOptions(output, "values", suspendWhenHidden = FALSE)

    ## Compute
    observe({
      req(ids())
      limits <- lapply(X = ids(), FUN = function(i, x) x[[i]], x = input)
      if (all(lengths(limits) != 0) || all(limits > 0)) {
        limits <- unlist(limits) / 100
        data$values <- nexus::replace_zero(
          x = x(),
          value = limits,
          delta = input$delta
        )
      }
    }) |>
      bindEvent(input$go)

    reactive({ data$values })
  })
}
