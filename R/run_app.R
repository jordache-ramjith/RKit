#' Launch RKit
#' @export
run_rkit <- function(...) {
  app_dir <- system.file("app", package = "Rkit", mustWork = TRUE)
  shiny::runApp(app_dir, ...)
}