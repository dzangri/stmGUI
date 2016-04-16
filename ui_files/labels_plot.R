fluidRow(
  column(5,
    wellPanel(
      fluidRow(
        column(8,
          actionButton("labelPlotCmd", "Generate Plot!")
        ),
        helpWithModal("labelPlotHelp", 4)
      ),
      tags$hr(),
      fluidRow(
        column(6,
          numericInput("labelPlotN", label = "n", value = 20)
        ),
        column(6,
          textInput("labelPlotTopics",
            label = "topics, comma separated (e.g. 1,2)",
            value = NULL)
        )
      ),
      fluidRow(
        column(6,
          radioButtons("labelPlotLabelType", label = "labeltype",
            choices = list("prob" = "prob", "frex" = "frex",
              "lift" = "lift", "score" = "score"), selected = "prob")
        ),
        column(6,
          numericInput("labelPlotFrexw",
            label = "frexw, only needed if frex is chosen for labeltype",
            value = 0.5, step = 0.1)
        )
      ),
      fluidRow(
        column(12,
          textInput("labelPlotMain", label = "main (title)", value = NULL)
        )
      ),
      fluidRow(
        column(6,
          textInput("labelPlotXLim",
            label = "x-axis limits, comma separated (e.g -.1,.1)",
            value = NULL)
        ),
        column(6,
          textInput("labelPlotYLim",
            label = "y-axis limits, comma separated (e.g -10,10)",
            value = NULL)
        )
      )
    )
  ),
  column(7,
    verbatimTextOutput("labelPlotTextResult"),
    plotOutput("labelPlot"),
    column(12,
      div(style = "text-align : right; padding-top : 20px;",
        downloadButton("labelsPlotDownload", "Download Plot as PDF")
      )
    )
  )
)