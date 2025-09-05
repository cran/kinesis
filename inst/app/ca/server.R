#' CA Shiny App Server Function
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
function(input, output, session) {
  ## Data
  data <- kinesis::count_server("prepare", demo = "zuni")

  ## CA
  results <- kinesis::ca_server("ca", x = data)

  kinesis::home_server("home")
  kinesis::footer_server("footer")
}
