fluidRow(
  column(5,
    wellPanel(
      fluidRow(
        column(8,
          actionButton("histPlotCmd", "Generate Plot!")
        ),
        helpWithModal("histPlotHelp", 4)
      ),
      tags$hr(),
      fluidRow(
        column(6,
          numericInput("histPlotN", label = "n", value = 3)
        ),
        column(6,
          textInput("histPlotTopics",
            label = "topics, comma separated (e.g. 1,2)",
            value = NULL)
        )
      ),
      fluidRow(
        column(6,
          radioButtons("histPlotLabelType", label = "labeltype",
            choices = list("prob" = "prob", "frex" = "frex",
              "lift" = "lift", "score" = "score"), selected = "prob")
        ),
        column(6,
          numericInput("histPlotFrexw",
            label = "frexw, only needed if frex is chosen for labeltype",
            value = 0.5, step = 0.1)
        )
      ),
      fluidRow(
        column(12,
          textInput("histPlotMain", label = "main (title)", value = NULL)
        )
      ),
      fluidRow(
        column(6,
          textInput("histPlotXLim",
            label = "x-axis limits, comma separated (e.g -.1,.1)",
            value = NULL)
        ),
        column(6,
          textInput("histPlotYLim",
            label = "y-axis limits, comma separated (e.g -10,10)",
            value = NULL)
        )
      )
    )
  ),
  column(7,
    verbatimTextOutput("histPlotTextResult"),
    plotOutput("histPlot"),
    column(12,
      div(style = "text-align : right; padding-top : 20px;",
        downloadButton("histPlotDownload", "Download Plot as PDF")
      )
    )
  )
)