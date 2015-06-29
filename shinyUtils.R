library(shiny)

clearInputGivenEvent <- function(inputName, outputToClear) {
#  observeEvent(inputName, ({
    outputToClear <- renderPrint({ invisible(NULL) })
#  }))
}