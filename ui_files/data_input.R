fluidPage(
  fluidRow(
    titleWithNextStep(
      "dataInputTitle",
      "Step 1: File Upload",
      "Proceed to Step 2: Processing"
    )
  ),
  fluidRow(
    column(5,
      wellPanel(
        fluidRow(
          column(9,
            fileInput("dataFileToUpload", "Choose Data File To Upload")
          ),
          helpWithModal("dataInputHelp", 3)
        ),
        hr(),
        actionButton("submitDataForUpload", "Submit"),
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
    ),
  column(7,
    verbatimTextOutput("dataInputTextResult"),
    DT::dataTableOutput('data', "50%")
  )
  )
)