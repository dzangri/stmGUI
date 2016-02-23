fluidRow(
  column(5,
    wellPanel(
      fluidRow(
        column(8, actionButton("stmRun", "Run STM")),
        helpWithModal("stmHelp", 4)
      ),
      fluidRow(
        tags$hr(),
        column(6,  numericInput("stmK", label = "K", value = NULL)),
        column(6,
          numericInput("stmSeed", label = "seed", value = NULL)
        )
      ),
      fluidRow(
        column(6,
          textInput("stmContent", label = "content formula\n(include leading ~)")
        ),
        column(6,
          textInput("stmPrev", label = "prevalence formula\n(include leading ~)")
        )
      ),
      fluidRow(
        column(12,
          radioButtons("stmInitType",
            label = "init type",
            choices = list("LDA" = "LDA", "Random" = "Random", "Spectral" = "Spectral"),
            inline = T,
            selected = "LDA")
        )
      ),
      hr(),
      a(id = "toggleAdvStm", "Show/hide advanced options"),
      shinyjs::hidden(
        div(id = "advStmOptions",
          br(),
          fluidRow(
            column(6,
              numericInput("stmMaxEm", label = "max em iterations", value = 100)
            ),
            column(6,
              numericInput("stmEmTol", label = "emtol", value = 0.00001, step = 0.00001)
            )
          ),
          fluidRow(
            column(6,
              numericInput("stmReportEvr", label = "report every", value = 1)
            ),
            column(6,
              radioButtons("stmLdaBeta", label = "LDA Beta",
                choices = list("True" = T, "False" = F), selected = T)
            )
          ),
          fluidRow(
            column(6,
              numericInput("stmNgroups", label = "ngroups", value = 1)
            ),
            column(6,
              radioButtons("stmInteractions", label = "Interactions",
                choices = list("True" = T, "False" = F), selected = T)
            )
          ),
          fluidRow(
            column(6,
              radioButtons("stmKappa", label = "Kappa Prior",
                choices = list("L1" = "L1", "Jeffreys" = "Jeffreys"),
                selected = "L1")
            ),
            column(6,
              radioButtons("stmGamma", label = "Gamma Prior",
                choices = list("Pooled" = "Pooled", "L1" = "L1"), selected = "Pooled")
            )
          ),
          fluidRow(
            column(12,
              sliderInput("stmSigma", "Sigma Prior",
                min = 0, max = 1, value = 0, step = 0.01)
            )
          )
        )
      )
    )
  ),
  column(7, verbatimTextOutput("stmTextResult"))
)