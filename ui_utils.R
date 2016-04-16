titleWithNextStep <- function(id, title, nextStepText) {
  ns <- NS(id)

  tagList(
    column(7, titlePanel(title)),
    column(5,
      div(style = "text-align : right; padding-top : 20px;",
        actionButton(
          ns("nextStep"),
          nextStepText
        )
      )
    )
  )
}

titleWithClearout <- function(id, title) {
  ns <- NS(id)

  tagList(
    column(8, titlePanel(title)),
    column(4,
      div(style = "text-align : right; padding-top : 20px;",
        actionButton(
          ns("clearout"),
          "Clear Output"
        )
      )
    )
  )
}

downloadPlotAsPdf <- function(id) {
  ns <- NS(id)

  tagList(
    column(12,
      div(style = "text-align : right; padding-top : 20px;",
        downloadButton(
          ns("downloadPdf"),
          "Download Plot As PDF"
        )
      )
    )
  )
}

helpWithModal <- function(id, helpSize) {
  ns <- NS(id)

  tagList(
    column(helpSize,
      div(style = "text-align : right;",
        actionButton(ns("help"), "", icon = icon("info-sign",
          lib = "glyphicon"))
      )
    ),
    bsTooltip(ns("help"), "Help", "top", options = list(container = "body")),
    bsModal(ns("modal"), "Help", ns("help"), size = "large",
      textOutput(ns("helpText"))
    )
  )
}