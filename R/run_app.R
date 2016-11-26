#' Helper function to launch the stm shiny app
#'
#' \code{runStmGui} launches the shiny app to help with walking through the
#' creation of stm models
#'
#' @import stm
#' @import tm
#'
#' @export
runStmGui <- function() {
  appDir <- system.file("app", package = "stmgui")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `stmgui`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}