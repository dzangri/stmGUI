#' @import markdown
#' @import shiny
#' @importFrom shinyjs useShinyjs hidden html onclick toggle toggleState enable disable
#' @import shinythemes
#' @import stm

titleWithNextStep <- function(id, title, nextStepText) {
  ns <- shiny::NS(id)

  shiny::tagList(column(7, titlePanel(title)),
                 column(5,
                        div(style = "text-align : right; padding-top : 20px;",
                            actionButton(ns("nextStep"), nextStepText))))
}

titleWithClearout <- function(id, title) {
  ns <- shiny::NS(id)

  shiny::tagList(column(8, titlePanel(title)),
                 column(4,
                        div(style = "text-align : right; padding-top : 20px;",
                            actionButton(ns("clearout"), "Clear Output"))))
}

downloadPlotAsPdf <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(column(12,
                        div(style = "text-align : right; padding-top : 20px;",
                            downloadButton(ns("downloadPdf"),
                                           "Download Plot As PDF"))))
}

helpWithModal <- function(id, helpSize) {
  ns <- shiny::NS(id)

  shiny::tagList(column(helpSize,
                        div(style = "text-align : right;",
                            actionButton(ns("help"),
                                         "",
                                         icon = icon("info-sign",
                                                     lib = "glyphicon")))),
                 shinyBS::bsTooltip(id=ns("help"),
                                    title="Help",
                                    placement="top",
                                    trigger="click",
                                    options = list(container = "body")),
                 shinyBS::bsModal(ns("modal"),
                                  "Help",
                                  ns("help"),
                                  size = "large",
                                  textOutput(ns("helpText"))))
}
