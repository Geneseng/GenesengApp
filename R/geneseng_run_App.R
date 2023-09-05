#' Run Geneseng App
#' 
#' @param port port
#' @param host host
#' 
#' @import shiny
#'
#' @examples
#' library(genesengApp)
#' geneseng_run_App()
#' 
#' 
#' @details
#' Solution found at https://stackoverflow.com/questions/37830819/developing-shiny-app-as-a-package-and-deploying-it-to-shiny-server
#'
#' @export
geneseng_run_App <- function(port = 3838, host = "0.0.0.0") {
  
  appDir <- system.file("app", package = "genesengApp")
  if (appDir == "") {
    stop("Could not find the app.", call. = FALSE)
  }
  
  runApp(appDir, display.mode = "normal", port = port, host = host)
  
}
