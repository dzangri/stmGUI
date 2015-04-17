library(markdown)
library(stm)

shinyUI(navbarPage("stm",
  
  # file upload tab
  tabPanel("Data",
    titlePanel("File Upload"),
    sidebarLayout(
      sidebarPanel(
        fileInput('datafile', 'Choose File'),
        tags$hr(),
        h6(paste("If your data is a csv or tsv, select the",
          "correct settings below, hit Submit, and a DataTable will",
          "apprear on the right.")),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                         c(Comma=',',
                         Semicolon=';',
                         Tab='\t'),
                       ','),
        radioButtons('quote', 'Quote',
                         c(None='',
                         'Double Quote'='"',
                         'Single Quote'="'"),
                       '"'),
        actionButton('filesettings', "Submit"),
        width = 3
        ),
    mainPanel(
      verbatimTextOutput("confirm"),
      dataTableOutput('data'),
      width = 9
      )
    )
  ),
  tabPanel("Processing",
    titlePanel("Process and Prep Documents"),
    fluidPage(
      fluidRow(
        h3("textProcessor"),
        fluidRow(
          column(5,
            wellPanel(
              fluidRow(
                column(6,
                  radioButtons("tpGadarian", label = h5("Choose textProcessor Datasource"),
                    choices = list("Gadarian Dataset" = 1, "Uploaded Data" = 2), 
                    selected = 1)
                )
              ),
              fluidRow(
                column(6,
                  textInput('tpDocs', label = h5("documents"))
                ),
                column(6,
                  textInput('tpMeta', label = h5("metadata"), value = NULL)
                )
              ),
              fluidRow(
                column(6,
                  radioButtons("tpLowercase", label = h5("lowercase"),
                    choices = list("True" = T, "False" = F), selected = T)
                ),
                column(6,
                  radioButtons("tpRemovestop", label = h5("remove stop words"),
                    choices = list("True" = T, "False" = F), selected = T)
                )
              ),
              fluidRow(
                column(6,
                  radioButtons("tpRemovenum", label = h5("remove numbers"),
                    choices = list("True" = T, "False" = F), selected = T)
                ),
                column(6,
                  radioButtons("tpRemovepunc", label = h5("remove punctuation"),
                    choices = list("True" = T, "False" = F), selected = T)
                )
              ),
              fluidRow(
                column(6,
                  radioButtons("tpStem", label = h5("stem"),
                    choices = list("True" = T, "False" = F), selected = T)
                ),
                column(6,
                  sliderInput("tpSparselevel", h5("sparselevel"),
                    min = 0, max = 1, value = 1, step = 0.05)
                )
              ),
              fluidRow(
                column(6,
                  textInput('tpLang', label = h5("language"), value = "en")
                ),
                column(6,
                  radioButtons("tpVerbose", label = h5("verbose"),
                    choices = list("True" = T, "False" = F), selected = T)
                )
              ),
              fluidRow(
                column(6,
                  radioButtons("tpOnlychar", label = h5("only character"),
                    choices = list("True" = T, "False" = F), selected = F)
                ),
                column(6,
                  radioButtons("tpStriphtml", label = h5("strip html"),
                    choices = list("True" = T, "False" = F), selected = F)
                )
              ),
              fluidRow(
                column(12,
                  textInput('tpCustomstop', label = h5("custom stopwords"), value = NULL)
                )
              ),
              fluidRow(
                column(12,
                  tags$hr(),
                  actionButton('tpTextprocess', "Process Text")
                )
              )
            )
          ),
          column(7,
            verbatimTextOutput("textprocresult")
          )
        )
      ),
      fluidRow(
        h3("plotRemoved"),
        fluidRow(
          column(5,
            wellPanel(
              fluidRow(
                column(6,
                  numericInput("plotLowThresh", label = h5("lower thresh"), value = 1)
                ),
                column(6,
                  numericInput("plotUpThresh", label = h5("upper thresh"), value = 200)
                )
              ),
              fluidRow(
                column(6,
                  numericInput("plotInterval", label = h5("interval"), value = 1)
                )
              ),
              fluidRow(
                column(12,
                  tags$hr(),
                  actionButton('plotPlotRemove', "Run plotRemove")
                )
              )
            )
          ),
          column(7,
            verbatimTextOutput("plotRemoveNoTP"),
            plotOutput("plotRemovePlot")
          )
        )
      ),
      fluidRow(
        h3("prepDocuments"),
        fluidRow(
          column(5,
            wellPanel(
              fluidRow(
                column(6,
                  radioButtons("pdChoice", label = h5("Choose data source"),
                    choices = list("textProcessor Output" = 1, "Raw Uploaded Data" = 2), 
                    selected = 1)
                )
              ),
              fluidRow(
                column(6,
                  textInput('pdDocs', label = h5("documents"))
                ),
                column(6,
                  textInput('pdVocab', label = h5("vocab"))
                )
              ),
              fluidRow(
                column(6,
                  textInput('pdMeta', label = h5("metadata"))
                )
              ),
              fluidRow(
                column(6,
                  numericInput("pdLowThresh", label = h5("lower thresh"), value = 1)
                ),
                column(6,
                  numericInput("pdUpThresh", label = h5("upper thresh"), value = .Machine$integer.max)
                )
              ),
              fluidRow(
                column(12,
                  tags$hr(),
                  actionButton('pdPrepdocs', "Prep Documents")
                )
              )
            )
          ),
          column(7,
            verbatimTextOutput("prepdocresults")
          )
        )
      )
    )
  ),
  tabPanel("Model",
    titlePanel("Enter Parameters and Run STM model"),
    fluidPage(
      fluidRow(
        h3("stm"),
        fluidRow(
          column(5,
            wellPanel(
              fluidRow(
                column(6,
                  radioButtons("stmChoice", label = h5("Choose data source"),
                    choices = list("prepDocuments Output" = 1, "Uploaded Data" = 2), 
                    selected = 1)
                )
              ),
              fluidRow(
                column(6,
                  textInput('stmDocs', label = h5("documents"))
                ),
                column(6,
                  textInput('stmVocab', label = h5("vocab"))
                )
              ),
              fluidRow(
                column(6,
                  numericInput("stmK", label = h5("K"), value = 3)
                ),
                column(6,
                  textInput('stmPrev', label = h5("prevalence"), value = NULL)
                )
              ),
              fluidRow(
                column(6,
                  textInput('stmContent', label = h5("content"), value = NULL)
                ),
                column(6,
                  textInput('stmData', label = h5("data"), value = NULL)
                )
              ),
              fluidRow(
                column(6,
                  radioButtons("stmInitType", label = h5("init type"),
                    choices = list("LDA" = 1, "Random" = 2, "Spectral" = 3), selected = 1)
                ),
                column(6,
                  numericInput("stmSeed", label = h5("seed"), value = 94301)
                )
              ),
              fluidRow(
                column(6,
                  numericInput("stmMaxEm", label = h5("max em iterations"), value = 10)
                ),
                column(6,
                  textInput('stmEmTol', label = h5("emtol"), value = "0.00001")
                )
              ),
              fluidRow(
                column(6,
                  radioButtons("stmVerbose", label = h5("verbose"),
                    choices = list("True" = TRUE, "False" = FALSE), selected = TRUE)
                )
              ),
              fluidRow(
                column(12,
                  tags$hr(),
                  actionButton('stmRun', "Run STM")
                )
              )
            )
          ),
          column(7,
            verbatimTextOutput("stmprocresult")
          )
        )
      )
    )
  ),
  tabPanel("Visualizations",
    titlePanel("Explore Results")
  )
))