fluidRow(
  column(5,
    wellPanel(
      fluidRow(
        column(6,
          actionButton("estEffPlot", "Generate Plot!")
        ),
        column(6,
          actionButton("estEffPlotClearout", "Clear Output")
        )
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
          textInput("estEffPlotTopics", label = "topics, comma-separated. Enter nothing to default ot all topics")
        )
      ),
      fluidRow(
        column(6,
          textInput("estEffCovVar1", label = "cov.value1, comma-separated")
        ),
        column(6,
          textInput("estEffCovVar2", label = "cov.value2, comma-separated")
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
      )
    )
  ),
  column(7,
    verbatimTextOutput("estEffPlotTextResult"),
    plotOutput("estEffPlot")
  )
)