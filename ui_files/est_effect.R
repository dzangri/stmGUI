fluidRow(
  column(5,
    wellPanel(
      fluidRow(
        column(8, actionButton("estEffRun", "Run estimateEffect")),
        helpWithModal("estEffHelp", 4)
      ),
      fluidRow(
        hr(),
        column(12,
          textInput("estEffFormula", label = "formula\n(include leading ~)")
        )
      ),
      fluidRow(
        column(12,
          radioButtons("estEffUncertainty",
            label = "uncertainty type",
            choices = list("Global" = "Global",
              "Local" = "Local",
              "None" = "None"),
            inline = T,
            selected = "Global")
        )
      ),
      hr(),
      a(id = "toggleAdvEstEff", "Show/hide advanced options"),
      shinyjs::hidden(
        div(id = "advEstEffOptions",
          br(),
          fluidRow(
            column(12,
              numericInput("estEffNSims", label = "nsims", value = 25)
            )
          ),
          fluidRow(
            column(12,
              textInput("estEffPrior", label = "prior, enter a scalar or comma-separated vector of numbers")
            )
          )
        )
      )
    )
  ),
  column(7, verbatimTextOutput("estEffTextResult"))
)