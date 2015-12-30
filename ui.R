##### stmGUI ####
#
# ui for running stm in Shiny
#
# **TODO**: separate required params from optional ones

library(shiny)
library(shinyjs)
library(markdown)
library(stm)

shinyUI(tagList(
  shinyjs::useShinyjs(),
  navbarPage("stm", id = "navBar", collapsible = TRUE,
    #### DataInput ####
    tabPanel("Data",
      fluidPage(
        fluidRow(
          column(6, titlePanel("Step 1: File Upload")),
          column(6,
            div(style = "text-align : right; padding-top : 20px;",
              actionButton("moveFromDataToProc", "Proceed to Step 2: Processing")
            )
          )
        ),
        fluidRow(
          column(5,
            wellPanel(
              div(id = "fileUploadOptionBar",
                br(),
                fileInput("dataFileToUpload", "Choose Data File To Upload"),
                actionButton("submitDataForUpload", "Submit"),
                hr(),
                a(id = "toggleAdvDataUpload", "Show/hide advanced options"),
                shinyjs::hidden(
                  div(id = "advUploadOptions",
                    checkboxInput("headerPresent", "Header Row Present", TRUE),
                    radioButtons("columnSeparator", "Separator Between Items In A Row",
                      c(Comma=",",
                        Semicolon=";",
                        Tab="\t"),
                      ","),
                    radioButtons("quoteAroundData", "Quote Around Data",
                      c(None="",
                        "Double Quote"='"',
                        "Single Quote"="'"),
                      '"')
                  )
                )
              )
            )),
          column(7,
            verbatimTextOutput("dataInputTextResult"),
            DT::dataTableOutput('data', "90%")
          )
        )
      )),
    ##### Processing #####
    tabPanel("Processing",
      fluidPage(
        fluidRow(
          column(8, titlePanel("Step 2: Process and Prep Documents")),
          column(4,
            div(style = "text-align : right; padding-top : 20px;",
              actionButton("moveFromProcToStm", "Proceed to Step 3: Generating STM Model")
            )
          )
        ),
        ##### Text Processor #####
        fluidRow(
          column(12, titlePanel("textProcessor"))
        ),
        fluidRow(
          column(5,
            wellPanel(
              fluidRow(
                column(6,
                  actionButton('tpProcessText', "Process Text")
                ),
                column(6,
                  actionButton('tpClearout', "Clear Output")
                )
              ),
              fluidRow(
                hr(),
                column(12,
                  radioButtons("tpGadarian",
                    label = "Choose textProcessor Datasource",
                    choices = list("Preloaded Gadarian Dataset" = "gadar", "Uploaded Data" = "upload"),
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
          )
        ),
        ##### Plot Removed #####
        fluidRow(
          column(12, titlePanel("plotRemoved"))
        ),
        fluidRow(
          column(5,
            wellPanel(
              fluidRow(
                column(6, actionButton("prRun", "Run plotRemove")),
                column(6, actionButton("prClearout", "Clear Output"))
              ),
              fluidRow(
                column(6,
                  tags$hr(),
                  numericInput("plotLowThresh", label = "lower thresh", value = 1)
                ),
                column(6,
                  tags$hr(),
                  numericInput("plotUpThresh", label = "upper thresh", value = 200)
                )
              ),
              fluidRow(
                column(6,
                  numericInput("plotInterval", label = "interval", value = 1)
                )
              )
            )
          ),
          column(7,
            verbatimTextOutput("prTextResult"),
            plotOutput("prPlotOutput", height = "350px")
          )
        ),
        ##### Prep Documents #####
        # **TODO**: Support user input that is prepdoc ready?
        fluidRow(
          column(12, titlePanel("prepDocuments"))
        ),
        fluidRow(
          column(5,
            wellPanel(
              fluidRow(
                column(6,
                  actionButton("pdPrepdocs", "Prep Documents")
                ),
                column(6,
                  actionButton("pdClearout", "Clear Output")
                )
              ),
              fluidRow(
                tags$hr(),
                column(6,
                  numericInput("pdLowThresh", label = "lower thresh", value = 1)
                ),
                column(6,
                  radioButtons("pdUpThreshChoice", label = "upper thresh",
                    choices = list("Inf" = "inf", "Manual Input" = "manual"),
                    selected = "inf"),
                  numericInput("pdUpThresh", "", value = Inf)
                )
              ),
              fluidRow(
                column(6,
                  numericInput("pdSubsample", label = "subsample", value = NULL)
                )
              )
            )
          ),
          column(7,
            verbatimTextOutput("pdTextResult")
          )
        )
      )
    ),
    ##### STM #####
    tabPanel("Model",
      fluidPage(
        fluidRow(
          column(12,
            titlePanel("Step 3: Run STM model")
          )
        ),
        fluidRow(
          column(12, titlePanel("stm"))
          #    column(2, actionButton("exportStm", "Export STM data"))
        ),
        fluidRow(
          column(5,
            wellPanel(
              fluidRow(
                column(6, actionButton("stmRun", "Run STM")),
                column(6, actionButton('stmClearout', "Clear STM Output"))
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
      )
    ),
    ##### Vizualizations #####
    navbarMenu("Plot",
      tabPanel("plot.STM",
        titlePanel("Choose options and plot the summary of an STM object"),
        fluidPage(
          fluidRow(
            column(9,
              h3("plot.STM")
            )
          ),
          fluidRow(
            column(5,
              wellPanel(
                fluidRow(
                  column(12,
                    radioButtons("plotStmType", label = "type",
                      choices = list("summary" = "summary", "labels" = "labels",
                        "perspectives" = "perspectives", "hist" = "hist"), selected = "summary")
                  )
                ),
                fluidRow(
                  column(6,
                    numericInput('plotStmN', label = "n", value = 3)
                  ),
                  column(6,
                    textInput('plotStmTopics',
                      label = "topics, comma separated (e.g. 1,2)",
                      value = NULL)
                  )
                ),
                fluidRow(
                  column(6,
                    radioButtons("plotStmLabelType", label = "labeltype",
                      choices = list("prob" = "prob", "frex" = "frex",
                        "lift" = "lift", "score" = "score"), selected = "prob")
                  ),
                  column(6,
                    numericInput("plotStmFrexw",
                      label = "frexw, only needed if frex is chosen for labeltype",
                      value = 0.5, step = 0.1)
                  )
                ),
                fluidRow(
                  column(12,
                    textInput("plotStmMain", label = "main (title)", value = NULL)
                  )
                ),
                fluidRow(
                  column(6,
                    textInput("plotStmXLim",
                      label = "x-axis limits, comma separated (e.g -.1,.1)",
                      value = NULL)
                  ),
                  column(6,
                    textInput('plotStmYLim',
                      label = "y-axis limits, comma separated (e.g -10,10)",
                      value = NULL)
                  )
                ),
                fluidRow(
                  column(6,
                    tags$hr(),
                    actionButton('plotStm', "Run STM plot!")
                  ),
                  column(6,
                    tags$hr(),
                    actionButton('plotStmClearout', "Clear Output")
                  )
                )
              )
            ),
            column(7,
              verbatimTextOutput("plotStmOut"),
              plotOutput("plotStmPlot")
            )
          )
        )
      ),
      tabPanel("labelTopics",
        titlePanel("Generate a set of words describing each topic from an STM object"),
        fluidPage(
          fluidRow(
            column(9,
              h3("labelTopics")
            )
          ),
          fluidRow(
            column(5,
              wellPanel(
                fluidRow(
                  column(6,
                    textInput('labelTopicsTopics',
                      label = "topics, comma separated (e.g. 1,2)",
                      value = NULL)
                  ),
                  column(6,
                    numericInput('labelTopicsN',
                      label = "n, number of words to label each topic",
                      value = 7)
                  )
                ),
                fluidRow(
                  column(6,
                    numericInput("labelTopicsFrexw",
                      label = "frexweight, a weight used in FREX scoring algorithm",
                      value = 0.5, step = 0.1)
                  )
                ),
                fluidRow(
                  column(6,
                    tags$hr(),
                    actionButton('labelTopics', "Run Label Topics")
                  ),
                  column(6,
                    tags$hr(),
                    actionButton('labelTopicsClearout', "Clear Output")
                  )
                )
              )
            ),
            column(7,
              verbatimTextOutput("labelTopicsOut"),
              plotOutput("labelTopicsPlot")
            )
          )
        )
      )
    )
  )))