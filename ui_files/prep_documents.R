fluidRow(
  column(5,
    wellPanel(
      fluidRow(
        column(6,
          actionButton("pdPrepdocs", "Prep Documents")
        ),
        column(6,
          actionButton("pdClearout", "Clear Output")
        )
      ),
      hr(),
      a(id = "toggleAdvPrepDocs", "Show/hide advanced options"),
      shinyjs::hidden(
        div(id = "advPrepDocsOptions",
          br(),
          fluidRow(
            column(6,
              numericInput("pdLowThresh", label = "lower thresh", value = 1)
            ),
            column(6,
              radioButtons("pdUpThreshChoice", label = "upper thresh",
                choices = list("Inf" = "inf", "Manual Input:" = "manual"),
                selected = "inf")
            )
          ),
          fluidRow(
            column(6,
              numericInput("pdSubsample", label = "subsample", value = NULL)
            ),
            column(6,
              numericInput("pdUpThresh", "", value = Inf)
            )
          )
        )
      )
    )
  ),
  column(7,
    verbatimTextOutput("pdTextResult")
  )
)