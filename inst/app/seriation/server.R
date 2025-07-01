#' Matrix Seriation Shiny App Server Function
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
function(input, output, session) {
  ## Data
  data <- kinesis::prepare_server("prepare", select = is.numeric, demo = "zuni")

  ## CA
  ca_results <- kinesis::ca_server("ca", x = data)

  ## Seriation
  seriation <- kinesis::seriate_server("seriate", x = data, order = ca_results)

  kinesis::home_server("home")
  kinesis::footer_server("footer")
  session$onSessionEnded(stopApp)
}
