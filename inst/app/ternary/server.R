#' Ternary Plot Shiny App Server Function
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
function(input, output, session) {
  ## Data
  data <- kinesis::prepare_server("prepare", demo = "bronze")

  ## Ternary Plot
  kinesis::ternary_server("ternary", x = data)

  kinesis::home_server("home")
  kinesis::footer_server("footer")
  session$onSessionEnded(stopApp)
}
