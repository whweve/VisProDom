#' @title a function to initiate shiny app VisDom
#' @description  to  initiate a shiny app designed to plot gene structure and protein domain for given gene across species
#' @author Hongwei Wang <\email{whweve@163.com}>
#' @export
#' @return a shiny app
#' @import shiny ggplot2 rintrojs dplyr data.table fresh shinyBS
runVPDapp <- function() {
  appDir <- system.file("VisProDom", package = "VisProDom")
  if (appDir == "") {
    stop("Could not find VisProDom. Try re-installing `VisProDom`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
