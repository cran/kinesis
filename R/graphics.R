# UI ===========================================================================
#' Graphical Parameters UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A [`list`] of UI elements.
#' @seealso [graphics_server()]
#' @family plot modules
#' @keywords internal
#' @export
graphics_ui <- function(id, col_quali = TRUE, col_quant = TRUE,
                        pch = TRUE, lty = TRUE, cex = TRUE, asp = FALSE) {
  ## Create a namespace function using the provided id
  ns <- NS(id)

  if (isTRUE(col_quali)) {
    col_quali <- select_color(
      inputId = ns("col_quali"),
      label = tr_("Qualitative scheme"),
      type = "quali"
    )
  } else {
    col_quali <- NULL
  }
  if (isTRUE(col_quant)) {
    col_quant <- select_color(
      inputId = ns("col_quant"),
      label = tr_("Quantitative scheme"),
      type = c("seq", "div")
    )
  } else {
    col_quant <- NULL
  }

  pch <- if (isTRUE(pch)) select_pch(ns("pch"), default = NULL) else NULL
  lty <- if (isTRUE(lty)) select_lty(ns("lty"), default = NULL) else NULL
  cex <- if (isTRUE(cex)) select_cex(ns("cex")) else NULL
  asp <- if (isTRUE(asp)) checkboxInput(ns("asp"), label = tr_("Fixed aspect ratio"), value = FALSE) else NULL

  list(col_quali, col_quant, pch, lty, cex, asp)
}

select_cex <- function(inputId, default = c(1, 2)) {
  sliderInput(
    inputId = inputId,
    label = tr_("Symbol size"),
    min = 0.1,
    max = 9,
    value = default,
    step = 0.1
  )
}

select_pch <- function(inputId, default = c(16, 17, 15, 3, 7, 8)) {
  x <- c(
    square = 0, circle = 1, `triangle up` = 2, plus = 3, cross = 4,
    diamond = 5, `triangle down` = 6, `square cross` = 7, star = 8,
    `diamond plus` = 9, `circle plus` = 10, `triangles up and down` = 11,
    `square plus` = 12, `circle cross` = 13, `square triangle` = 14,
    `filled square` = 15, `filled circle` = 16, `filled triangle` = 17,
    `filled diamond` = 18, `solid circle` = 19, bullet = 20
  )

  selectizeInput(
    inputId = inputId,
    label = tr_("Symbol"),
    choices = x,
    selected = default,
    multiple = TRUE,
    options = list(plugins = "clear_button")
  )
}

select_lty <- function(inputId, default = 1) {
  x <- c(solid = 1, dashed = 2, dotted = 3,
         dotdash = 4, longdash = 5, twodash = 6)

  selectizeInput(
    inputId = inputId,
    label = tr_("Line type"),
    choices = x,
    selected = default,
    multiple = TRUE,
    options = list(plugins = "clear_button")
  )
}

select_color <- function(inputId, label,
                         type = c("qualitative", "sequential", "diverging")) {
  type <- match.arg(type, several.ok = TRUE)

  schemes <- list(
    qualitative = c("discreterainbow", "bright", "vibrant", "muted",
                    "highcontrast", "mediumcontrast", "light", "okabeito"),
    diverging = c("sunset", "nightfall", "BuRd", "PRGn"),
    sequential = c("YlOrBr", "iridescent", "incandescent", "smoothrainbow")
  )

  schemes <- schemes[type]
  default <- "discreterainbow"
  if (length(type) == 1) {
    if ("diverging" %in% type) default <- "BuRd"
    if ("sequential" %in% type) default <- "YlOrBr"
  }

  selectizeInput(
    inputId = inputId,
    label = label,
    choices = schemes,
    selected = default,
    multiple = FALSE,
    options = list(plugins = list("clear_button"))
  )
}

# Server =======================================================================
#' Graphical Parameters Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @return
#'  A [`reactiveValues`][shiny::reactiveValues] object with elements:
#'  \describe{
#'   \item{`col_quali`, `col_quant`}{A palette function that when called with a
#'    single argument returns a character vector of colors.}
#'   \item{`pch`}{A palette function that when called with a single argument
#'    returns a character vector of symbols.}
#'   \item{`lty`}{A palette function that when called with a single argument
#'    returns a character vector of symbols.}
#'   \item{`cex`}{A palette function that when called with a single argument
#'    returns a numeric vector giving the amount by which plotting text and
#'    symbols should be magnified relative to the default.}
#'   \item{`asp`}{}
#'  }
#' @seealso [graphics_ui()]
#' @family plot modules
#' @keywords internal
#' @export
graphics_server <- function(id) {

  moduleServer(id, function(input, output, session) {
    param <- reactiveValues(
      col_quali = recycle("black"),
      col_quant = recycle("black"),
      pch = recycle(16),
      lty = recycle(1),
      cex = recycle(1),
      asp = NA
    )

    observe({
      if (isTRUE(input$asp)) {
        param$asp <- 1
      } else {
        param$asp <- NA
      }
    }) |>
      bindEvent(input$asp)

    observe({
      param$pal_quali <- color(input$col_quali)
      param$col_quali <- protect(khroma::palette_color_discrete, "black", param$pal_quali)
    }) |>
      bindEvent(input$col_quali, ignoreNULL = FALSE)

    observe({
      param$pal_quant <- color(input$col_quant)
      param$col_quant <- protect(khroma::palette_color_continuous, "black", param$pal_quant)
    }) |>
      bindEvent(input$col_quant, ignoreNULL = TRUE)

    observe({
      pch <- as.integer(input$pch) %|||% 16
      param$pal_pch <- input$pch
      if (isTruthy(input$pch)) {
        param$pch <- protect(khroma::palette_shape, pch[[1L]], pch)
      } else {
        param$pch <- recycle(pch[[1L]])
      }
    }) |>
      bindEvent(input$pch, ignoreNULL = FALSE)

    observe({
      lty <- as.integer(input$lty) %|||% 1
      param$pal_lty <- input$lty
      if (isTruthy(input$lty)) {
        param$lty <- protect(khroma::palette_line, lty[[1L]], lty)
      } else {
        param$lty <- recycle(lty[[1L]])
      }
    }) |>
      bindEvent(input$lty, ignoreNULL = FALSE)

    observe({
      cex <- range(as.integer(input$cex)) %|||% 1
      param$pal_cex <- input$cex
      if (isTruthy(input$cex)) {
        param$cex <- protect(khroma::palette_size_sequential, cex[[1L]], cex)
      } else {
        param$cex <- recycle(cex[[1L]])
      }
    }) |>
      bindEvent(input$cex)

    param
  })
}

color <- function(scheme, default = "black") {
  if (!isTruthy(scheme)) {
    function(n) {
      rep(default, n)
    }
  } else {
    function(n) {
      notify(khroma::color(scheme)(n))
    }
  }
}
recycle <- function(x) {
  force(x)

  function(n) {
    if (missing(n) || length(n) < 1) n <- 1
    rep(x, length(n))
  }
}
protect <- function(f, default, ...) {
  force(default)

  function(x) {
    if (!isTruthy(x)) return(default)
    notify(f(...)(x))
  }
}

