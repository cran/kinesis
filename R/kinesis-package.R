#' @details
#'  \tabular{ll}{
#'   **Version** \tab 0.2.0 \cr
#'   **License** \tab GPL-3 \cr
#'   **Zenodo DOI** \tab \doi{10.5281/zenodo.14645671} \cr
#'  }
#'
#'  Archéosciences Bordeaux (UMR 6034)\cr
#'  Maison de l'Archéologie\cr
#'  Université Bordeaux Montaigne\cr
#'  F-33607 Pessac cedex\cr
#'  France
#'
#' @section Package options:
#'  \pkg{kinesis} uses the following [options()] to configure behavior:
#'  * `kinesis.workers`: an [`integer`] specifying the number of \R sessions
#'  to be used for asynchronous (parallel) computing. Defaults to 1.
#'
#' @name kinesis-package
#' @aliases kinesis-package kinesis
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @import shiny
#' @import bslib
NULL

# Suppress R CMD check note "All declared Imports should be used."
unused <- function() {
  folio::boves
  datasets::iris
}
