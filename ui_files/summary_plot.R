fluidRow(
  column(5,
    wellPanel(
      fluidRow(
        column(8,
          actionButton("summaryPlotCmd", "Generate Plot!")
        ),
        helpWithModal("summaryPlotHelp", 4)
      ),
      tags$hr(),
      fluidRow(
        column(6,
          numericInput("summaryPlotN", label = "n", value = 3)
        ),
        column(6,
          textInput("summaryPlotTopics",
            label = "topics, comma separated (e.g. 1,2)",
            value = NULL)
        )
      ),
      fluidRow(
        column(6,
          radioButtons("summaryPlotLabelType", label = "labeltype",
            choices = list("prob" = "prob", "frex" = "frex",
              "lift" = "lift", "score" = "score"), selected = "prob")
        ),
        column(6,
          numericInput("summaryPlotFrexw",
            label = "frexw, only needed if frex is chosen for labeltype",
            value = 0.5, step = 0.1)
        )
      ),
      fluidRow(
        column(12,
          textInput("summaryPlotMain", label = "main (title)", value = NULL)
        )
      ),
      fluidRow(
        column(6,
          textInput("summaryPlotXLim",
            label = "x-axis limits, comma separated (e.g -.1,.1)",
            value = "")
        ),
        column(6,
          textInput("summaryPlotYLim",
            label = "y-axis limits, comma separated (e.g -10,10)",
            value = "")
        )
      )
    )
  ),
  column(7,
    verbatimTextOutput("summaryPlotTextResult"),
    plotOutput("summaryPlot")
  )
)