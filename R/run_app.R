#' Run the Shiny app
#' @import ggplot2
#' @export
run_spade <- function(...) {
  app_dir <- system.file("shiny", "elicitation_app", package = "spade")
  if (app_dir == "") stop("Could not find app directory. Is the package installed?")
  shiny::runApp(app_dir, ...)
}

#' Title
#'
#' @returns
#' @export
#'
#' @examples
run_tutorial_1 <- function() {
  app_dir <- system.file("shiny", "tutorial_1", package = "spade")

  if (app_dir == "") {
    stop("Could not find the tutorial directory. Try re-installing the package.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
