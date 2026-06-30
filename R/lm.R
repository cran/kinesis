# UI ===========================================================================
#' Linear Model UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [lm_server()]
#' @family modeling modules
#' @keywords internal
#' @export
lm_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Linear Model"),
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = tr_("Linear Model"),
        ## Input: select axes
        selectize_ui(
          id = ns("response"),
          label = tr_("Dependent variable"),
          multiple = FALSE
        ),
        selectize_ui(
          id = ns("explanatory"),
          label = tr_("Independent variable"),
          multiple = FALSE
        ),
        selectize_ui(
          id = ns("quali"),
          label = tr_("Extra qualitative variable")
        ),
        selectize_ui(
          id = ns("group"),
          label = tr_("Group")
        )
      ), # sidebar
      navset_card_pill(
        nav_panel(
          title = tr_("Prediction"),
          layout_sidebar(
            sidebar = sidebar(
              ## Input: prediction
              radioButtons(
                inputId = ns("interval"),
                label = tr_("Interval"),
                choiceNames = c(tr_("Confidence interval"), tr_("Prediction interval")),
                choiceValues = c("confidence", "prediction")
              ),
              radioButtons(
                inputId = ns("level"),
                label = tr_("Level:"),
                selected = "0.95",
                choiceNames = c("68%", "95%", "99%"),
                choiceValues = c("0.68", "0.95", "0.99")
              )
            ),
            layout_columns(
              col_widths = c(8, 4),
              output_plot(
                id = ns("plot_lm"),
                title = tr_("Plot"),
                tools = list(
                  graphics_ui(
                    id = ns("par"), col_quali = FALSE, col_quant = FALSE,
                    pch = FALSE, lty = FALSE, cex = FALSE, asp = TRUE
                  ),
                  checkboxInput(inputId = ns("grid"), label = tr_("Grid"), value = TRUE)
                )
              ),
              gt::gt_output(outputId = ns("prediction"))
            )
          )
        ),
        nav_panel(
          title = tr_("Summary"),
          verbatimTextOutput(outputId = ns("summary"))
        ),
        nav_panel(
          title = tr_("Diagnostic"),
          layout_columns(
            col_widths = breakpoints(xs = 12, sm = c(6, 6), md = c(4, 4, 4)),
            output_plot(id = ns("plot_hist"), title = tr_("Residuals histogram")),
            output_plot(id = ns("plot_qq"), title = tr_("Residual Q-Q plot")),
            output_plot(id = ns("plot_fitted"), title = tr_("Residuals-Fitted")),
            output_plot(id = ns("plot_scale"), title = tr_("Scale-Location")),
            output_plot(id = ns("plot_cook"), title = tr_("Cook's distance")),
            output_plot(id = ns("plot_lev"), title = tr_("Residuals-Leverage"))
          )
        )
      ) # navset_card_pill
    ) # layout_sidebar
  ) # nav_panel
}

# Server =======================================================================
#' Linear Model Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @param x A reactive [`data.frame`].
#' @return A reactive [`lm`] object.
#' @seealso [lm_ui()]
#' @family modeling modules
#' @keywords internal
#' @export
lm_server <- function(id, x) {
  stopifnot(is.reactive(x))

  moduleServer(id, function(input, output, session) {
    ## Update UI -----
    quanti <- subset_quantitative(x)
    quali <- subset_qualitative(x)

    col_quali <- update_selectize_colnames("quali", x = quali)
    resp <- update_selectize_colnames("response", x = quanti)
    expl <- update_selectize_colnames("explanatory", x = quanti, exclude = resp)

    ## Subset -----
    groups <- select_data(quali, col_quali, drop = TRUE)
    group <- update_input("group", x = groups, control = updateSelectizeInput,
                          choices = unique, select = FALSE, placeholder = TRUE)
    data <- reactive({
      if (!isTruthy(group())) return(x())
      x()[which(groups() == group()), , drop = FALSE]
    })

    ## Linear regression -----
    vars <- reactive({
      req(resp(), expl())
      stats::as.formula(paste0(resp(), " ~ ", paste0(expl(), collapse = " + ")))
    }) |>
      bindEvent(expl()) |>
      debounce(500)

    model <- reactive({
      notify(
        stats::lm(vars(), data = data(), na.action = stats::na.omit, y = TRUE)
      )
    }) |>
      bindEvent(vars(), data())

    prediction <- reactive({
      pred <- notify(
        stats::predict(
          object = model(),
          se.fit = FALSE,
          interval = input$interval,
          level = as.numeric(input$level)
        )
      )
      data.frame(y = model()$y, pred)
    })

    ## Graphical parameters -----
    param <- graphics_server("par")

    ## Build plot -----
    plot_lm <- reactive({
      ## Select data
      req(data(), prediction())

      coord_x <- data()[[expl()]]
      coord_y <- data()[[resp()]]

      ## Build plot
      function() {
        graphics::plot(
          x = coord_x,
          y = coord_y,
          type = "p",
          xlab = expl(),
          ylab = resp(),
          panel.first = if (isTRUE(input$grid)) graphics::grid() else NULL,
          col = "black",
          pch = 16,
          cex = 1,
          asp = param$asp,
          las = 1
        )

        i <- order(coord_x)
        graphics::polygon(
          x = c(coord_x[i], rev(x = coord_x[i])),
          y = c(prediction()$upr[i], rev(prediction()$lwr[i])),
          col = grDevices::adjustcolor("grey", alpha.f = 0.5), border = NA
        )
        graphics::lines(x = coord_x[i], y = prediction()$upr[i],
                        lty = 2, lwd = 1, col = "#004488")
        graphics::lines(x = coord_x[i], y = prediction()$lwr[i],
                        lty = 2, lwd = 1, col = "#004488")
        graphics::lines(x = coord_x[i], y = prediction()$fit[i],
                        lty = 1, lwd = 1, col = "#BB5566")
      }
    })

    ## Diagnostic tests -----
    # TODO?

    ## Diagnostic plots -----
    plot_hist <- reactive({
      function() {
        graphics::hist(stats::residuals(model()), main = NULL,
                       xlab = tr_("Residuals"), ylab = tr_("Frequency"))
      }
    })
    plot_fitted <- reactive({
      function() {
        plot(model(), which = 1, caption = "", sub.caption = "")
      }
    })
    plot_qq <- reactive({
      function() {
        plot(model(), which = 2, caption = "", sub.caption = "")
      }
    })
    plot_scale <- reactive({
      function() {
        plot(model(), which = 3, caption = "", sub.caption = "")
      }
    })
    plot_cook <- reactive({
      function() {
        plot(model(), which = 4, caption = "", sub.caption = "")
      }
    })
    plot_lev <- reactive({
      function() {
        plot(model(), which = 5, caption = "", sub.caption = "")
      }
    })

    ## Render plot -----
    render_plot("plot_lm", plot_lm)
    render_plot("plot_hist", plot_hist)
    render_plot("plot_fitted", plot_fitted)
    render_plot("plot_qq", plot_qq)
    render_plot("plot_scale", plot_scale)
    render_plot("plot_cook", plot_cook)
    render_plot("plot_lev", plot_lev)

    ## Render table -----
    output$prediction <- gt::render_gt({
      lvl <- as.numeric(input$level)
      int <- switch(
        input$interval,
        confidence = tr_("Confidence interval"),
        prediction = tr_("Prediction interval")
      )
      gt::gt(prediction(), rownames_to_stub = TRUE) |>
        gt::tab_spanner(
          label = sprintf("%s (%1.0f%%)", int, lvl * 100),
          columns = c("lwr", "upr")
        ) |>
        gt::cols_label(
          y = tr_("Response"),
          fit = tr_("Fitted"),
          lwr = tr_("Lower bound"),
          upr = tr_("Upper bound")
        )
    })

    ## Render prints -----
    output$summary <- renderPrint({ summary(model()) })

    model
  })
}
