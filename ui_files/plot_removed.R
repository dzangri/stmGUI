fluidRow(
  column(5,
    wellPanel(
      fluidRow(
        column(8, actionButton("prRun", "Run plotRemove")),
        helpWithModal("prHelp", 4)
      )
      #                   fluidRow(
      #                     column(6,
      #                       tags$hr(),
      #                       numericInput("plotLowThresh", label = "lower thresh", value = 1)
      #                     ),
      #                     column(6,
      #                       tags$hr(),
      #                       numericInput("plotUpThresh", label = "upper thresh", value = 200)
      #                     )
      #                   ),
      #                   fluidRow(
      #                     column(6,
      #                       numericInput("plotInterval", label = "interval", value = 1)
      #                     )
      #                   )
    )
  ),
  column(7,
    #                verbatimTextOutput("prTextResult"),
    plotOutput("prPlotOutput", height = "350px"),
    div(style = "align : center;",
      sliderInput("prPlotRange", "Range:",
        min = 1, max = 1000, value = c(1,200), width = "100%")
    )
  )
)