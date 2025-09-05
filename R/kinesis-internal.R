# HELPERS

## https://michaelchirico.github.io/potools/articles/developers.html
tr_ <- function(...) {
  enc2utf8(gettext(paste0(...), domain = "R-kinesis"))
}

#' Bootstrap Theme
#'
#' @param version A [`character`] string specifying the major version of
#'  Bootstrap to use.
#' @param ... Extra parameters to be passed to [bslib::bs_theme()].
#' @return
#'  Returns a [sass::sass_bundle()] object (see [bslib::bs_theme()]).
#' @keywords internal
#' @export
theme_ui <- function(version = "5", ...) {
  path_style <- system.file("static", "custom.scss", package = "kinesis")
  scss <- sass::sass_file(path_style)

  bs <- bslib::bs_theme(
    version = version,
    base_font = c("sans-serif"),
    heading_font = c("sans-serif"),
    code_font = NULL,
    ...
  )
  bslib::bs_add_rules(bs, scss)
}

# Helpers ======================================================================
validate_csv <- function(x) {
  validate(need(x, message = tr_("Import a CSV file first.")))
}
validate_rows <- function(x, n = 1) {
  rows <- ngettext(n, "Select at least %d row.", "Select at least %d rows.")
  validate(need(NROW(x) >= n, sprintf(rows, n)), errorClass = "kinesis")
}
validate_columns <- function(x, n = 1) {
  cols <- ngettext(n, "Select at least %d column.", "Select at least %d columns.")
  validate(need(NCOL(x) >= n, sprintf(cols, n)), errorClass = "kinesis")
}
validate_dim <- function(x, i = 1, j = 1) {
  validate_rows(x, n = i)
  validate_columns(x, n = j)
}
validate_na <- function(x) {
  validate(need(!anyNA(x), tr_("Your data should not contain missing values.")),
           errorClass = "kinesis")
}
validate_zero <- function(x) {
  validate(need(all(x != 0), tr_("Your data should not contain zeros.")),
           errorClass = "kinesis")
}

#' Default Value for Falsy
#'
#' Replaces a [falsy][shiny::isTruthy] value with a default value.
#' @param x,y An object.
#' @return If `x` is not [truthy][shiny::isTruthy()], returns `y`;
#'  otherwise returns `x`.
#' @keywords internal
#' @name falsy
#' @rdname falsy
`%|||%` <- function(x, y) {
  if (isTruthy(x)) x else y
}

# Widgets ======================================================================
select_calendar <- function(id, default = "CE") {
  ns <- NS(id)

  selectizeInput(
    inputId = ns("calendar"),
    label = tr_("Calendar"),
    choices = c("CE", "BCE", "BP", "AD", "BC"),
    selected = default,
    multiple = FALSE,
    options = list(plugins = "remove_button")
  )
}
get_calendar <- function(id) {
  moduleServer(id, function(input, output, session) {

    cal <- reactive({
      aion::calendar(input$calendar)
    })

    cal
  })
}

#' Build Numeric Input
#'
#' @param id A [`character`] string specifying the namespace.
#' @param x A reactive `data.frame` (typically returned by [import_server()]).
#' @return
#'  * `build_numeric_input()` returns a reactive [`numeric`] vector
#'    (side effect: render numeric input controls).
#'  * `render_numeric_input()` is called for its side effects
#'    (creates UI elements).
#' @keywords internal
build_numeric_input <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Get variable names
    vars <- reactive({ names(x()) })

    ## Build UI
    output$controls <- renderUI({
      lapply(
        X = vars(),
        FUN = function(var) {
          numericInput(
            inputId = session$ns(paste0("num_", var)),
            label = var,
            value = 0
          )
        }
      )
    })

    ## Get values
    values <- reactive({
      vapply(
        X = paste0("num_", vars()),
        FUN = function(var, input) input[[var]],
        FUN.VALUE = numeric(1),
        input = input
      )
    })

    values
  })
}

#' @rdname build_numeric_input
render_numeric_input <- function(id) {
  uiOutput(NS(id, "controls"))
}

# Notification =================================================================
show_notification <- function(text, title = NULL, id = NULL, duration = 5,
                              closeButton = TRUE, type = "default") {
  # text <- paste0(text, collapse = "\n")
  if (!is.null(title)) text <- sprintf("**%s**\n%s", title, text)
  id <- showNotification(
    ui = markdown(text, hardbreaks = TRUE),
    duration = duration,
    closeButton = closeButton,
    id = id,
    type = type
  )
  invisible(id)
}

#' Notify
#'
#' Shows a notification if an expression raises an error or a warning.
#' @param expr An expression to be evaluated.
#' @param what A [`character`] string giving the title of the notification.
#' @return The result of `expr` or `NULL`.
#' @keywords internal
#' @noRd
notify <- function(expr, title = NULL) {
  warn <- err <- NULL

  res <- withCallingHandlers(
    tryCatch(
      expr,
      error = function(e) {
        if (!inherits(e, "shiny.silent.error")) { # Ignore silent error
          err <<- conditionMessage(e)
        }
        return(NULL)
      }
    ),
    warning = function(w) {
      warn <<- append(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (!is.null(err)) {
    show_notification(text = err, title = title, type = "error")
  }
  if (!is.null(warn)) {
    warn <- unique(warn)
    show_notification(text = warn, title = title, type = "warning")
  }

  res
}

#' Compare Two \R Objects
#'
#' Shows a notification if `x` and `y` are not [identical][identical()].
#' @param x A reactive object.
#' @param y A reactive object.
#' @param title A [`character`] string giving the title of the notification.
#' @return
#'  No return value, called for side effects.
#' @keywords internal
#' @noRd
notify_change <- function(id, x, y, title = "Important message") {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(y))

  moduleServer(id, function(input, output, session) {
    observe({
      if (identical(x(), y())) {
        removeNotification(id)
      } else {
        txt <- paste(tr_("Your data seem to have changed."),
                     tr_("You should perform your analysis again."), sep = " ")
        show_notification(id = id, text = txt, title = title,
                          duration = NULL, closeButton = FALSE,
                          type = "warning")
      }
    }) |>
      bindEvent(x(), y())
  })
}
