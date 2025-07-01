#' Aoristic Analysis Shiny App Server Function
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
function(input, output, session) {
  ## Data
  data <- kinesis::prepare_server("prepare", demo = "loire")

  ## Analysis
  intervals <- kinesis::time_interval_server("intervals", x = data)
  results <- kinesis::aoristic_server("aoristic", x = intervals, y = data)

  kinesis::home_server("home")
  kinesis::footer_server("footer")
  session$onSessionEnded(stopApp)
}
