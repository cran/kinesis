#' Scatter Plot Shiny App Server Function
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
function(input, output, session) {
  ## Data
  data <- kinesis::prepare_server("prepare", demo = "iris")

  ## Plot
  kinesis::scatter_server("scatter", x = data)

  ## Linear model
  model <- kinesis::lm_server("lm", x = data)

  kinesis::home_server("home")
  kinesis::footer_server("footer")
}
