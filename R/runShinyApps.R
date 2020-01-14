#' @export
runApp_CompareSiteVsDatim <- function(){
  appDir <- system.file("siteVsDatimApp", package = "datapackr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
    }
  shiny::runApp(appDir, display.mode = "normal")
  }