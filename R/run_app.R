#' Launch RKit
#' @export
run_rkit <- function(...) {
  app_dir <- system.file("app", package = "RKit", mustWork = TRUE)
  shiny::runApp(app_dir, launch.browser = TRUE, ...)
}