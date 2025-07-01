#' Diversity Shiny App Server Function
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

  ## Plot
  kinesis::bertin_server("bertin", x = data)

  ## Diversity
  alpha <- kinesis::diversity_alpha_server("alpha", x = data)
  kinesis::diversity_beta_server("beta", x = data, y = alpha)
  kinesis::occurrence_server("occurrence", x = data)

  kinesis::home_server("home")
  kinesis::footer_server("footer")
  session$onSessionEnded(stopApp)
}
