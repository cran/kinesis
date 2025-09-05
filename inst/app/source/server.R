#' Compositional Data Analysis Shiny App Server Function
#'
#' @param input Provided by \pkg{Shiny}.
#' @param output Provided by \pkg{Shiny}.
#' @param session Provided by \pkg{Shiny}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
function(input, output, session) {
  ## Data
  coda <- kinesis::coda_server("coda", demo = "bronze")

  ## Statistics
  kinesis::coda_summary_server("coda_summary", coda)
  # kinesis::coda_outliers_server("outliers", coda)

  ## Graphs
  kinesis::coda_hist_server("hist", x = coda)
  kinesis::coda_barplot_server("barplot", x = coda)
  kinesis::ternary_server("ternary", x = coda)

  ## Log-ratio
  clogratio <- kinesis::logratio_server("clr", coda, method = "clr")
  alogratio <- kinesis::logratio_server("alr", coda, method = "alr")
  ilogratio <- kinesis::logratio_server("ilr", coda, method = "ilr")
  plogratio <- kinesis::logratio_server("plr", coda, method = "plr")

  ## PCA
  pca_results <- kinesis::pca_server("pca", x = clogratio)

  ## Cluster
  clust <- kinesis::coda_hclust_server("clust", x = coda)

  kinesis::home_server("home")
  kinesis::footer_server("footer")
}
