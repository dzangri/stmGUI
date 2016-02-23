fluidRow(
  column(5,
    wellPanel(
      fluidRow(
        column(8,
          actionButton("labelTopics", "Run Label Topics")
        ),
        helpWithModal("labelTopicsHelp", 4)
      ),
      hr(),
      fluidRow(
        column(6,
          textInput('labelTopicsTopics',
            label = "topics, comma separated (e.g. 1,2)",
            value = NULL)
        ),
        column(6,
          numericInput('labelTopicsN',
            label = "n, number of words to label each topic",
            value = 7)
        )
      ),
      fluidRow(
        column(6,
          numericInput("labelTopicsFrexw",
            label = "frexweight, a weight used in FREX scoring algorithm",
            value = 0.5, step = 0.1)
        )
      )
    )
  ),
  column(7,
    verbatimTextOutput("labelTopicsTextResult"),
    plotOutput("labelTopicsPlot")
  )
)