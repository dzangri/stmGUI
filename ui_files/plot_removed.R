fluidRow(
  column(5,
    wellPanel(
      fluidRow(
        column(8, actionButton("prRun", "Run plotRemove")),
        helpWithModal("prHelp", 4)
      )
    )
  ),
  column(7,
    #                verbatimTextOutput("prTextResult"),
    plotOutput("prPlotOutput", height = "350px"),
    div(style = "align : center;",
      sliderInput(
        "prPlotRange",
        "Lower threshold values to test word and document removal from the sample:",
        min = 1, max = 1000, value = c(1,200), width = "100%")
    )
  )
)