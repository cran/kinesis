.onLoad <- function(libname, pkgname) {
  path <- system.file("static", package = "kinesis")
  shiny::addResourcePath(prefix = "static", directoryPath = path)

  invisible()
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("static")

  invisible()
}
