# SHINY

#' Run an App
#'
#' A wrapper for [shiny::shinyAppDir()].
#' @param app A [`character`] string specifying the \pkg{Shiny} application
#'  to run (see details). Any unambiguous substring can be given.
#' @param bookmark A [`logical`] scalar: should server-side bookmarking of the
#'  application be enabled (see [shiny::enableBookmarking()])?
#' @param options A [`list`] of named options that should be passed to the
#'  [`shiny::shinyAppDir()`] call.
#' @details
#'  \tabular{ll}{
#'   **Application name**          \tab  **Keyword** \cr
#'   Aoristic Analysis             \tab `aoristic`   \cr
#'   Correspondence Analysis       \tab `ca`         \cr
#'   Principal Components Analysis \tab `pca`        \cr
#'   Diversity Measures            \tab `diversity`  \cr
#'   Mean Ceramic Date             \tab `mcd`        \cr
#'   Scatter Plot                  \tab `scatter`    \cr
#'   Matrix Seriation              \tab `seriation`  \cr
#'   Compositional Data Analysis   \tab `source`     \cr
#'   Ternary Plot                  \tab `ternary`    \cr
#'  }
#' @examples
#' if (interactive()) {
#'  run_app("seriation")
#' }
#' @return A \pkg{shiny} application object.
#' @family shiny apps
#' @author N. Frerebeau
#' @export
run_app <- function(app = c("diversity", "seriation", "aoristic", "mcd",
                            "source", "scatter", "ternary", "ca", "pca"),
                    bookmark = FALSE,
                    options = list(launch.browser = interactive())) {
  ## App selection
  app <- match.arg(app, several.ok = FALSE)
  app_dir <- system.file("app", app, package = "kinesis")
  if (app_dir == "") {
    msg <- sprintf(tr_("Could not find the %s application."), sQuote(app))
    stop(msg, call. = FALSE)
  }

  ## Enable bookmarking
  bookmark <- isTRUE(bookmark)
  shiny::enableBookmarking(store = ifelse(bookmark, "server", "disable"))

  ## Create a Shiny app object
  obj <- shiny::shinyAppDir(appDir = app_dir, options = options)

  ## Bundle the options inside the shinyApp object
  opt <- get_config(app, file = NULL)
  opt$bookmark <- bookmark
  obj$appOptions$kinesis_options <- opt

  obj
}

#' Read Configuration Values
#'
#' @param app A [`character`] string specifying the Shiny application
#'  to run (see [run_app()]).
#' @param file A [`character`] string specifying the configuration file to
#'  read from. If `NA` (the default), use the value of the
#'  `KINESIS_CONFIG_FILE` environment variable ("`config.yml`" if the variable
#'  does not exist). If `NULL`, use the build-in configuration file.
#' @param active A [`character`] string specifying the name of configuration to
#'  read from. If `NA` (the default), use the value of the
#'  `KINESIS_CONFIG_ACTIVE` environment variable ("`default`" if the variable
#'  does not exist).
#' @param use_parent A [`logical`] scalar: should parent directories be scanned
#'  for configuration files if the specified config file isn't found?
#' @return A [`list`] of configuration values.
#' @author N. Frerebeau
#' @keywords internal
#' @export
get_config <- function(app, file = NA, active = NA, use_parent = TRUE) {
  ## Get config file
  if (is.null(file)) {
    file <- system.file("app", app, "config.yml", package = "kinesis")
  }
  if (is.na(file)) {
    file <- Sys.getenv("KINESIS_CONFIG_FILE", "config.yml")
  }
  if (!file.exists(file)) {
    msg <- sprintf(tr_("Could not find the configuration file for %s."), sQuote(app))
    stop(msg, call. = FALSE)
  }

  ## Read config
  if (is.na(active)) {
    active <- Sys.getenv("KINESIS_CONFIG_ACTIVE", "default")
  }
  config::get(value = NULL, config = active, file = file,
              use_parent = use_parent)
}

#' Get App Options
#'
#' @param name A [`character`] string specifying the name of an option to get.
#'  If `NULL` (the default), all options are returned.
#' @param default A value to be returned if the option is not currently set.
#' @return
#'  The value of a \pkg{Shiny} option (see [shiny::getShinyOption()]).
#' @author N. Frerebeau
#' @keywords internal
#' @export
get_option <- function(name = NULL, default = NULL) {
  if (is.null(name)) {
    shiny::getShinyOption("kinesis_options")
  } else {
    shiny::getShinyOption("kinesis_options")[[name]] %||% default
  }
}

#' Get Current Language
#'
#' @param default A [`character`] string specifying the default language
#'  (ISO 639-2) if [`Sys.getenv("LANGUAGE")`][Sys.getenv] is not set. If `NULL`
#'  (the default), uses [`Sys.getlocale("LC_COLLATE")`][Sys.getlocale].
#' @return A [`character`] string (ISO 639-2).
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
get_language <- function(default = NULL) {
  ## Get current language
  lang <- Sys.getenv("LANGUAGE", unset = NA)
  if (is.na(lang) || nchar(lang) < 2)
    lang <- default %||% Sys.getlocale("LC_COLLATE")
  substr(lang, start = 1, stop = 2)
}

#' Get App Title
#'
#' @param default A [`character`] string specifying the default language
#'  (see [get_language()]).
#' @return A [`character`] string.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
get_title <- function(default = NULL) {
  lang <- get_language(default)
  title <- get_option("title")[[lang]]
  if (is.null(title)) title <- get_option("title")[["en"]] # Fallback to English
  title
}

#' Get App Description
#'
#' @param default A [`character`] string specifying the default language
#'  (see [get_language()]).
#' @return A [`character`] string.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
get_description <- function(default = NULL) {
  lang <- get_language(default)
  desc <- get_option("description")[[lang]]
  if (is.null(desc)) desc <- get_option("description")[["en"]] # Fallback to English
  desc
}
