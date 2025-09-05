# Utilities ====================================================================
select_data <- function(x, names, drop = FALSE) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(names))

  reactive({
    if (!all(dim(x()) > 0)) return(NULL)

    cols <- arkhe::get_columns(x = x(), names = names())
    if (!isTRUE(drop)) return(cols)
    unlist(cols, use.names = FALSE)
  }) |>
    bindEvent(names())
}
subset_data <- function(x, f = NULL) {
  stopifnot(is.reactive(x))

  reactive({
    req(x())
    if (!is.function(f)) return(x())
    arkhe::keep_columns(x = x(), f = f, verbose = get_option("verbose", FALSE))
  })
}
subset_quantitative <- function(x, positive = FALSE) {
  if (isTRUE(positive)) {
    f <- \(x) is.numeric(x) && all(x >= 0, na.rm = TRUE)
  } else {
    f <- is.numeric
  }
  subset_data(x, f)
}
subset_qualitative <- function(x) {
  f <- \(x) Negate(is.numeric)(x)
  subset_data(x, f)
}

# UI ===========================================================================
#' Updatable Select List
#'
#' @param id A [`character`] string specifying the namespace.
#' @return
#'  A select list control that can be added to a UI definition
#'  (see [shiny::selectizeInput()]).
#' @seealso [update_selectize_colnames()], [update_selectize_rownames()]
#' @keywords internal
selectize_ui <- function(id, label = "Choose", multiple = FALSE) {
  ns <- NS(id)
  plugins <- ifelse(isTRUE(multiple), "remove_button", "clear_button")
  options <- list(plugins = plugins)

  selectizeInput(
    inputId = ns("names"),
    label = label,
    choices = NULL,
    selected = NULL,
    multiple = multiple,
    options = options
  )
}

#' Updatable Checkbox Group
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A checkbox group control that can be added to a UI definition
#'  (see [shiny::checkboxGroupInput()]).
#' @seealso [update_checkbox_colnames()]
#' @keywords internal
checkbox_ui <- function(id, label = "Choose", inline = TRUE) {
  ns <- NS(id)

  checkboxGroupInput(
    inputId = ns("names"),
    label = label,
    choices = NULL,
    selected = NULL,
    inline = inline
  )
}

# Server =======================================================================
#' Update a Checkbox Group with Column Names
#'
#' @inheritParams update_input
#' @return
#'  A reactive [`character`] vector of column names.
#'
#'  Side effect: change the value of a checkbox group on the client.
#' @seealso [checkbox_ui()]
#' @keywords internal
update_checkbox_colnames <- function(id, x, exclude = reactive(NULL), select = TRUE) {
  update_input(id = id, x = x, exclude = exclude, select = select,
               control = updateCheckboxGroupInput)
}

#' Update a Select List with Column Names
#'
#' @inheritParams update_input
#' @return
#'  A reactive [`character`] vector of column names.
#'
#'  Side effect: change the value of a select list on the client.
#' @seealso [selectize_ui()]
#' @keywords internal
update_selectize_colnames <- function(id, x, exclude = reactive(NULL),
                                      select = FALSE, placeholder = TRUE) {
  update_input(id = id, x = x, exclude = exclude, select = select,
               placeholder = placeholder,
               control = updateSelectizeInput)
}

#' Update a Select List with Row Names
#'
#' @inheritParams update_input
#' @return
#'  A reactive [`character`] vector of row names.
#'
#'  Side effect: change the value of a select list on the client.
#' @seealso [selectize_ui()]
#' @keywords internal
update_selectize_rownames <- function(id, x, exclude = reactive(NULL),
                                      select = FALSE, placeholder = TRUE) {
  update_input(id = id, x = x, choices = rownames, exclude = exclude,
               select = select, placeholder = placeholder,
               control = updateSelectizeInput)
}

#' Update an Input Control with Column Names
#'
#' @param control An UI input updater.
#' @param id A [`character`] string specifying the namespace.
#' @param x A reactive `matrix`-like object.
#' @param choices A [`function`] that takes `x` as a single argument and returns
#'  a `character` vector.
#' @param exclude A reactive [`character`] vector of values to be excluded from
#'  choices.
#' @param select A [`logical`] scalar: should all choices be selected?
#' @param placeholder A [`logical`] scalar: should a placeholder be added?
#' @return
#'  A reactive [`character`] vector of column names.
#'
#'  Side effect: change the value of an input control on the client.
#' @seealso [update_checkbox_colnames()], [update_selectize_colnames()],
#'  [update_selectize_rownames()]
#' @keywords internal
update_input <- function(control, id, x,
                         choices = colnames, exclude = reactive(NULL),
                         select = TRUE, placeholder = FALSE) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(exclude))

  moduleServer(id, function(input, output, session) {
    trigger <- reactive({ exclude() %|||% x() })

    ## Update UI
    observe({
      opt <- choices(x())
      opt <- setdiff(opt, exclude())
      selected <- if (isTRUE(select)) opt else NULL

      if (length(opt) > 0) {
        ## Try to keep previous selection, if any
        keep <- intersect(opt, input$names)
        if (length(keep) > 0) selected <- keep
      } else {
        opt <- character(0)
      }

      ## Add placeholder
      if (isTRUE(placeholder)) {
        opt <- c("", opt)
        names(opt) <- c(tr_("Choose"), rep("", length(opt) - 1))
      }

      freezeReactiveValue(input, "names")
      control(
        session,
        inputId = "names",
        choices = opt,
        selected = selected
      )
    }) |>
      bindEvent(trigger())

    ## Return variable names
    reactive({ input$names })
  })
}
