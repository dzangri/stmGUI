fluidRow(
  column(5,
    wellPanel(
      fluidRow(
        column(8,
          actionButton("doEstEffPlot", "Generate Plot!")
        ),
        helpWithModal("estEffPlotHelp", 4)
      ),
      tags$hr(),
      fluidRow(
        column(6,
          textInput("estEffPlotCov",
            label = "covariate",
            value = NULL)
        ),
        column(6,
          radioButtons("estEffPlotMethod", label = "method",
            choices = list("pointestimate" = "pointestimate",
              "difference" = "difference",
              "continuous" = "continuous"),
            selected = "pointestimate")
        )
      ),
      fluidRow(
        column(12,
          textInput("estEffPlotTopics", label = "topics, comma-separated.
            Enter nothing to default to all topics")
        )
      ),
      fluidRow(
        column(6,
          textInput("estEffCovVar1", label = "cov.value1")
        ),
        column(6,
          textInput("estEffCovVar2", label = "cov.value2")
        )
      ),
      fluidRow(
        column(6,
          textInput("estEffMod", label = "moderator")
        ),
        column(6,
          textInput("estEffModValue", label = "moderator.value")
        )
      ),
      fluidRow(
        column(6,
          textInput("estEffLineCol", label = "linecol")
        ),
        column(6,
          numericInput("estEffNPoints", label = "npoints", value = 100)
        )
      ),
      fluidRow(
        column(6,
          textInput("estEffXLim", label = "xlim, comma separated (e.g -.1,.1)")
        ),
        column(6,
          textInput("estEffYLim", label = "ylim, comma separated (e.g -10,10)")
        )
      )
    )
  ),
  column(7,
    verbatimTextOutput("estEffPlotTextResult"),
    plotOutput("estEffPlot"),
    column(12,
      div(style = "text-align : right; padding-top : 20px;",
        downloadButton("estEffPlotDownload", "Download Plot as PDF")
      )
    )
  )
)