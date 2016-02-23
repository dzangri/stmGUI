fluidRow(
  column(5,
    wellPanel(
      fluidRow(
        column(8,
          actionButton("perspPlotCmd", "Generate Plot!")
        ),
        helpWithModal("perspPlotHelp", 4)
      ),
      tags$hr(),
      fluidRow(
        column(6,
          numericInput("perspPlotN", label = "n", value = 25)
        ),
        column(6,
          textInput("perspPlotTopics",
            label = "topics, comma separated (e.g. 1,2)",
            value = NULL)
        )
      ),
      fluidRow(
        column(6,
          radioButtons("perspPlotLabelType", label = "labeltype",
            choices = list("prob" = "prob", "frex" = "frex",
              "lift" = "lift", "score" = "score"), selected = "prob")
        ),
        column(6,
          numericInput("perspPlotFrexw",
            label = "frexw, only needed if frex is chosen for labeltype",
            value = 0.5, step = 0.1)
        )
      ),
      fluidRow(
        column(12,
          textInput("perspPlotMain", label = "main (title)", value = NULL)
        )
      ),
      fluidRow(
        column(6,
          textInput("perspPlotXLim",
            label = "x-axis limits, comma separated (e.g -.1,.1)",
            value = NULL)
        ),
        column(6,
          textInput("perspPlotYLim",
            label = "y-axis limits, comma separated (e.g -10,10)",
            value = NULL)
        )
      )
    )
  ),
  column(7,
    verbatimTextOutput("perspPlotTextResult"),
    plotOutput("perspPlot")
  )
)