fluidRow(
  column(5,
    wellPanel(
      fluidRow(
        column(8,
          actionButton('tpProcessText', "Process Text")
        ),
        helpWithModal("tpHelp", 4)
      ),
      fluidRow(
        hr(),
        column(12,
          radioButtons("tpGadarian",
            label = "Choose textProcessor Datasource",
            choices =
              list(
                "Preloaded Gadarian Dataset" = "gadar",
                "Uploaded Data" = "upload"
              ),
            selected = "gadar")
        )
      ),
      fluidRow(
        column(12,
          selectInput("tpDocs", "Name of column containing text documents", c())
        )
      ),
      hr(),
      a(id = "toggleAdvTextProc", "Show/hide advanced options"),
      shinyjs::hidden(
        div(id = "advTextProcOptions",
          br(),
          fluidRow(
            column(6,
              radioButtons("tpLowercase", label = "lowercase",
                choices = list("True" = T, "False" = F), selected = T)
            ),
            column(6,
              radioButtons("tpRemovestop", label = "remove stop words",
                choices = list("True" = T, "False" = F), selected = T)
            )
          ),
          fluidRow(
            column(6,
              radioButtons("tpRemovenum", label = "remove numbers",
                choices = list("True" = T, "False" = F), selected = T)
            ),
            column(6,
              radioButtons("tpRemovepunc", label = "remove punctuation",
                choices = list("True" = T, "False" = F), selected = T)
            )
          ),
          fluidRow(
            column(6,
              radioButtons("tpStem", label = "stem",
                choices = list("True" = T, "False" = F), selected = T)
            ),
            column(6,
              sliderInput("tpSparselevel", "sparselevel",
                min = 0, max = 1, value = 1, step = 0.05)
            )
          ),
          fluidRow(
            column(12,
              selectInput("tpLang",
                label = "language",
                choices = SnowballC::getStemLanguages(),
                selected = "english")
            )
          ),
          fluidRow(
            column(6,
              radioButtons("tpOnlychar", label = "only character",
                choices = list("True" = T, "False" = F), selected = F)
            ),
            column(6,
              radioButtons("tpStriphtml", label = "strip html",
                choices = list("True" = T, "False" = F), selected = F)
            )
          ),
          fluidRow(
            column(12,
              textInput('tpCustomstop', label = "custom stopwords", value = NULL)
            )
          )
        )
      )
    )
  ),
  column(7,
    verbatimTextOutput("tpTextResult")
    #                 plotOutput("prPlotOutput", height = "350px"),
    #                 div(style = "align : center;",
    #                   sliderInput("prPlotRange", "Range:",
    #                     min = 1, max = 1000, value = c(1,200), width = "100%")
    #                 )
  )
)