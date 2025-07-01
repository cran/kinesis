# UI ===========================================================================
#' About UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @param package A [`character`] vector of package names to be cited.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @seealso [home_server()]
#' @family page modules
#' @keywords internal
#' @export
home_ui <- function(id, package) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Home"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = tr_("Welcome!"),
        help_tesselle(),
        help_license()
      ), # sidebar
      navset_card_pill(
        placement = "above",
        nav_panel(
          title = tr_("Overview"),
          h3(get_title()),
          markdown(get_description()),
          help_data(),
          help_warranty(),
          tags$p(
            class = "logo",
            tags$a(href = "https://www.tesselle.org/", rel = "external",
                   tags$img(src = "static/tesselle.png", alt = tr_("Logo of the tesselle project."))),
            tags$a(href = "https://www.archeosciences-bordeaux.fr/", rel = "external",
                   tags$img(src = "static/logo-archeosciences.svg", alt = "UMR 6034 Arch&eacute;osciences Bordeaux")),
            tags$a(href = "https://www.huma-num.fr/", rel = "external",
                   tags$img(src = "static/logo-humanum.svg", alt = "IR* Huma-Num"))
          )
        ),
        nav_panel(
          title = tr_("How to cite"),
          help_cite(package)
        ),
        nav_panel(
          title = tr_("Bookmark"),
          tags$p(
            tr_("You can save the state of the application and get a URL which will restore the application with that state."),
            tr_("You can then copy the URL and save it for later, or share it with others so they can visit the application in the bookmarked state.")
          ),
          tags$p(
            tr_("This is not intended for long-term storage."),
            tr_("There is no guarantee as to how long your bookmark will last.")
          ),
          if (get_option("bookmark", FALSE)) {
            tags$div(class = "d-grid d-md-block", bookmarkButton())
          } else {
            tags$p(tr_("Bookmarking is currently disabled."))
          }
        )
      ) # navset_card_pill
    ) # layout_sidebar
  ) # nav_panel
}

#' Footer UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A [`list`] that can be converted into an HTML `<footer>` tag
#'  (see [htmltools::tags()]).
#' @seealso [footer_server()]
#' @family page modules
#' @keywords internal
#' @export
footer_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tags$footer(
    style = "border-top: 1px; margin-top: 1em; width: 100%; text-align: center;",
    tags$p(
      actionLink(inputId = ns("session"), label = tr_("Session info")),
      HTML(" &middot; "),
      tags$a(href = "https://codeberg.org/tesselle/kinesis",
             target = "_blank", rel = "external", tr_("Source code")),
      HTML(" &middot; "),
      tags$a(href = "https://codeberg.org/tesselle/kinesis/issues",
             target = "_blank", rel = "external", tr_("Report a bug or request"))
    )
  )
}

# Server =======================================================================
#' Home Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @return
#'  No return value, called for side effects.
#' @seealso [home_ui()]
#' @family page modules
#' @keywords internal
#' @export
home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Bookmark -----
    onBookmark(function(state) {
      saved_time <- Sys.time()

      msg <- sprintf(tr_("Last saved at %s."), saved_time)
      showNotification(
        ui = msg,
        duration = 5,
        closeButton = TRUE,
        type = "message",
        session = session
      )
      message(msg)

      # state is a mutable reference object,
      # we can add arbitrary values to it.
      state$values$time <- saved_time
    })

    onRestore(function(state) {
      msg <- sprintf(tr_("Restoring from state bookmarked at %s."), state$values$time)
      showNotification(
        ui = msg,
        duration = 5,
        closeButton = TRUE,
        type = "message",
        session = session
      )
      message(msg)
    })

    ## Render -----
    output$session <- renderPrint({ utils::sessionInfo() })
  })
}

#' Footer Server
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'  UI function.
#' @return
#'  No return value, called for side effects.
#' @seealso [footer_ui()]
#' @family page modules
#' @keywords internal
#' @export
footer_server  <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$session, {
      showModal(
        modalDialog(
          title = tr_("Session info"),
          info_session(),
          size = "xl",
          easyClose = TRUE,
          footer = modalButton(tr_("Close"))
        )
      )
    })
  })
}

#' Collect Information About the Current R Session
#'
#' @param ... Currently not used.
#' @return Text marked as HTML.
#' @keywords internal
#' @noRd
info_session <- function(...) {
  info <- paste0(utils::capture.output(utils::sessionInfo()), collapse = "\n")
  markdown(sprintf("```\n%s\n```", info))
}
