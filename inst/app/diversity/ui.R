#' Diversity Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
function(request) {
  bslib::page_navbar(
    title = toupper(kinesis::get_option("name")),
    id = "main",
    kinesis::home_ui("home", package = "tabula"),
    kinesis::prepare_ui("prepare"),
    kinesis::bertin_ui("bertin"),
    kinesis::occurrence_ui("occurrence"),
    kinesis::diversity_alpha_ui("alpha"),
    kinesis::diversity_beta_ui("beta"),
    kinesis::diversity_docs_ui("docs"),
    bslib::nav_spacer(),
    bslib::nav_item(bslib::input_dark_mode()),
    footer = kinesis::footer_ui("footer"),
    navbar_options = bslib::navbar_options(collapsible = TRUE),
    theme = kinesis::theme_ui(),
    lang = "en"
  )
}
