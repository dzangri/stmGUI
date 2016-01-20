fluidPage(
  fluidRow(
    column(6, titlePanel("Step 1: File Upload")),
    column(6,
      div(style = "text-align : right; padding-top : 20px;",
        actionButton("moveFromDataToProc", "Proceed to Step 2: Processing")
      )
    )
  ),
  fluidRow(
    column(5,
      wellPanel(
        div(id = "fileUploadOptionBar",
          br(),
          fileInput("dataFileToUpload", "Choose Data File To Upload"),
          bsTooltip("viewDataHelp", "Help",
            "top", options = list(container = "body")),
          bsModal("dataHelpModal", "Help", "viewDataHelp", size = "large",
            verbatimTextOutput("dataHelp")),
          br(),
          actionButton("submitDataForUpload", "Submit"),
          actionButton("viewDataHelp", "", icon = icon("info-sign",
            lib = "glyphicon")),
          hr(),
          a(id = "toggleAdvDataUpload", "Show/hide advanced options"),
          shinyjs::hidden(
            div(id = "advUploadOptions",
              checkboxInput("headerPresent", "Header Row Present", TRUE),
              radioButtons("columnSeparator",
                "Separator Between Items In A Row",
                c(Comma=",",
                  Semicolon=";",
                  Tab="\t"),
                ","),
              radioButtons("quoteAroundData", "Quote Around Data",
                c(None="",
                  "Double Quote"='"',
                  "Single Quote"="'"),
                '"')
            )
          )
        )
      )
    ),
    column(7,
      verbatimTextOutput("dataInputTextResult"),
      DT::dataTableOutput('data', "90%")
    )
  )
)