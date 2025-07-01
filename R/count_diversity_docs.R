# UI ===========================================================================
#' Diversity Definitions UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @return
#'  A nav item that may be passed to a nav container
#'  (e.g. [bslib::navset_tab()]).
#' @family count data modules
#' @keywords internal
#' @export
diversity_docs_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  nav_panel(
    title = tr_("Definitions"),
    layout_columns(
      col_widths = breakpoints(xs = c(12, 12), lg = c(6, 6)),
      card(
        card_title(tr_("Alpha Diversity")),
        tags$dl(
          tags$dt(tr_("Heterogeneity index")),
          tags$dd(tr_("The higher the heterogeneity value, the more diverse the individuals are in the dataset.")),
          tags$dl(
            tags$dt("Shannon"),
            tags$dd(
              "Shannon, C. E. (1948). A Mathematical Theory of Communication.",
              tags$em("The Bell System Technical Journal"), "27, 379-423.",
              url_doi("10.1002/j.1538-7305.1948.tb01338.x", prefix = TRUE)
            ),
            tags$dt("Brillouin"),
            tags$dd(
              "Brillouin, L. (1956).", tags$em("Science and information theory."), "New York: Academic Press."
            )
          ),

          tags$dt(tr_("Dominance index")),
          tags$dd(tr_("Dominance is a measure of whether a community is dominated by certain types (an increase in the value means a decrease in diversity).")),
          tags$dl(
            tags$dt("Simpson"),
            tags$dd(
              "Simpson, E. H. (1949). Measurement of Diversity.",
              tags$em("Nature"), "163(4148), 688-688.",
              url_doi("10.1038/163688a0", prefix = TRUE)
            ),
            tags$dt("Berger-Parker"),
            tags$dd(
              "Berger, W. H. & Parker, F. L. (1970). Diversity of Planktonic Foraminifera in Deep-Sea Sediments.",
              tags$em("Science"), "168(3937), 1345-1347.",
              url_doi("10.1126/science.168.3937.1345", prefix = TRUE)
            )
          ),

          tags$dt(tr_("Richness index")),
          tags$dd(tr_("Richness quantifies how many different types the dataset of interest contains, it does not take into account the abundances of the types.")),
          tags$dl(
            tags$dt("Menhinick"),
            tags$dd(
              "Menhinick, E. F. (1964). A Comparison of Some Species-Individuals Diversity Indices Applied to Samples of Field Insects.",
              tags$em("Ecology"), "45(4), 859-861.",
              url_doi("10.2307/1934933", prefix = TRUE)
            ),
            tags$dt("Margalef"),
            tags$dd(
              "Margalef, R. (1958). Information Theory in Ecology.",
              tags$em("General Systems"), "3, 36-71."
            ),
            tags$dt("Chao 1"),
            tags$dd(
              "Chao, A. (1984). Nonparametric Estimation of the Number of Classes in a Population.",
              tags$em("Scandinavian Journal of Statistics"), "11(4), 265-270."
            ),
            tags$dt("ACE"),
            tags$dd(
              "Chao, A. & Lee, S.-M. (1992). Estimating the Number of Classes via Sample Coverage.",
              tags$em("Journal of the American Statistical Association"), "87(417), 210-217.",
              url_doi("10.1080/01621459.1992.10475194", prefix = TRUE)
            ),
            tags$dt("Squares Estimator"),
            tags$dd(
              "Alroy, J. (2018). Limits to Species Richness in Terrestrial Communities.",
              tags$em("Ecology Letters"), "21(12), 1781-1789.",
              url_doi("10.1111/ele.13152", prefix = TRUE)
            )
          )
        )
      ),
      card(
        card_title(tr_("Beta Diversity")),
        tags$dl(
          tags$dt("Bray-Curtis"),
          tags$dd(
            "Bray, J. R. & Curtis, J. T. (1957). An Ordination of the Upland Forest Communities of Southern Wisconsin.",
            tags$em("Ecological Monographs"), "27(4), 325-349.",
            url_doi("10.2307/1942268", prefix = TRUE)
          ),
          tags$dt("Dice-Sorensen"),
          tags$dd(
            "Dice, L. R. (1945). Measures of the Amount of Ecologic Association Between Species.",
            tags$em("Ecology"), "26(3): 297-302.",
            url_doi("10.2307/1932409", prefix = TRUE)
          ),
          tags$dd(
            "Sorensen, T. (1948). A Method of Establishing Groups of Equal Amplitude in Plant Sociology Based on Similarity of Species Content and Its Application to Analyses of the Vegetation on Danish Commons.",
            tags$em("Kongelige Danske Videnskabernes Selskab"), "5(4): 1-34."
          ),
          tags$dt("Morisita-Horn (Horn modified version of the Morisita index)"),
          tags$dd(
            "Horn, H. S. (1966). Measurement of \"Overlap\" in Comparative Ecological Studies.",
            tags$em("The American Naturalist"), "100(914): 419-424.",
            url_doi("10.1086/282436", prefix = TRUE)
          ),
          tags$dd(
            "Mosrisita, M. (1959). Measuring of interspecific association and similarity between communities.",
            tags$em("Memoirs of the Faculty of Science, Kyushu University, Series E"), "3:65-80."
          )
        )
      )
    ) # layout_columns
  ) # nav_panel
}
