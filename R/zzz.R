.onLoad <- function(libname, pkgname) {
  path <- system.file("static", package = "kinesis")
  shiny::addResourcePath(prefix = "static", directoryPath = path)

  op <- options()
  op.kinesis <- list(
    kinesis.workers = 1
  )
  toset <- !(names(op.kinesis) %in% names(op))
  if(any(toset)) options(op.kinesis[toset])

  invisible()
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("static")

  invisible()
}
