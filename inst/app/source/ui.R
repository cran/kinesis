#' Compositional Data Analysis Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
function(request) {
  bslib::page_navbar(
    title = toupper(kinesis::get_option("name")),
    id = "main",
    kinesis::home_ui("home", package = "nexus"),
    kinesis::coda_ui("coda"),
    kinesis::coda_summary_ui("coda_summary"),
    bslib::nav_menu(
      title = "Plot",
      kinesis::coda_barplot_ui("barplot"),
      kinesis::ternary_ui("ternary")
    ),
    bslib::nav_menu(
      title = "Transform",
      kinesis::logratio_ui("clr", title = "CLR"),
      kinesis::logratio_ui("alr", title = "ALR"),
      kinesis::logratio_ui("ilr", title = "ILR"),
      kinesis::logratio_ui("plr", title = "PLR")
    ),
    bslib::nav_menu(
      title = "Analysis",
      # kinesis::coda_outliers_ui("outliers"),
      kinesis::pca_ui("pca", scale = FALSE),
      kinesis::coda_hclust_ui("clust")
    ),
    bslib::nav_spacer(),
    bslib::nav_item(bslib::input_dark_mode()),
    footer = kinesis::footer_ui("footer"),
    navbar_options = bslib::navbar_options(collapsible = TRUE),
    theme = kinesis::theme_ui(),
    lang = "en"
  )
}
