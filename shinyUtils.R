library(shiny)

clearInputGivenEvent <- function(inputName, outputToClear) {
  observe({
    inputName
    outputToClear <- renderPrint({ invisible(NULL) })
  })
}