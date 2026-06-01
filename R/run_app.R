#' Run the Shiny app
#' @export
run_spade <- function(...) {
  app_dir <- system.file("shiny", "elicitation_app", package = "spade")
  if (app_dir == "") stop("Could not find app directory. Is the package installed?")
  shiny::runApp(app_dir, ...)
}
