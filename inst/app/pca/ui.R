#' PCA Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
function(request) {
  bslib::page_navbar(
    title = toupper(kinesis::get_option("name")),
    id = "main",
    kinesis::home_ui("home", package = "dimensio"),
    kinesis::prepare_ui("prepare"),
    kinesis::pca_ui("pca"),
    bslib::nav_spacer(),
    bslib::nav_item(bslib::input_dark_mode()),
    footer = kinesis::footer_ui("footer"),
    navbar_options = bslib::navbar_options(collapsible = TRUE),
    theme = kinesis::theme_ui(),
    lang = "en"
  )
}
