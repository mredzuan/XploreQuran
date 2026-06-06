#' Run the Shiny Application
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
run_app <- function() {
  app_dir <- system.file("app", package = "XploreQuran")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `XploreQuran`.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
