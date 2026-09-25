#' Launch the statistics learning app
#' @export
run_mmbs_app <- function(...) {
  shiny::runApp(system.file("app", package = "mmbslearn"), ...)
}
