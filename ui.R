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
                fileInput("dataFileToUpload", "Choose Data File To Upload"),
                actionButton("submitDataForUpload", "Submit"),
                hr(),
                a(id = "toggleAdvDataUpload", "Show/hide advanced options"),
                shinyjs::hidden(
                  div(id = "advUploadOptions",
                    checkboxInput("headerPresent", "Header", TRUE),
                    radioButtons("columnSeparator", "Separator",
                      c(Comma=",",
                        Semicolon=";",
                        Tab="\t"),
                      ","),
                    radioButtons("quoteAroundData", "Quote",
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
                tags$hr(),
                column(12,
                  radioButtons("tpGadarian", label = h5("Choose textProcessor Datasource"),
                    choices = list("Preloaded Gadarian Dataset" = "gadar", "Uploaded Data" = "upload"),
                    selected = "gadar")
                )
              ),
              fluidRow(
                column(12,
                  selectInput("tpDocs", label = h5("Name of column containing text documents"), choices = c())
                )
              ),
              hr(),
              a(id = "toggleAdvTextProc", "Show/hide advanced options"),
              shinyjs::hidden(
                div(id = "advTextProcOptions",
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
                  numericInput("plotLowThresh", label = h5("lower thresh"), value = 1)
                ),
                column(6,
                  tags$hr(),
                  numericInput("plotUpThresh", label = h5("upper thresh"), value = 200)
                )
              ),
              fluidRow(
                column(6,
                  numericInput("plotInterval", label = h5("interval"), value = 1)
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
                  actionButton('pdPrepdocs', "Prep Documents")
                ),
                # **TODO**: Decide if 'Clear Output' is too vague, since just referring to output text
                column(6,
                  actionButton('pdClearout', "Clear Output")
                )
              ),
              fluidRow(
                tags$hr(),
                column(6,
                  numericInput("pdLowThresh", label = h5("lower thresh"), value = 1)
                ),
                column(6,
                  radioButtons("pdUpThreshChoice", label = h5("Upper Thresh"),
                    choices = list("Inf" = "inf", "Manual Input" = "manual"),
                    selected = "inf"),
                  numericInput("pdUpThresh", label = h5(""), value = Inf)
                )
              ),
              fluidRow(
                column(6,
                  numericInput("pdSubsample", label = h5("subsample"), value = NULL)
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
      titlePanel("Step 3: Run STM model"),
      fluidPage(
        fluidRow(
          column(10, h3("stm"))
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
                column(6,  numericInput("stmK", label = h5("K"), value = NULL)),
                column(6,
                  textInput("stmPrev", label = h5("prevalence formula, include leading ~"))
                )
              ),
              fluidRow(
                column(6,
                  textInput("stmContent", label = h5("content"))
                ),
                column(6,
                  numericInput("stmSeed", label = h5("seed"), value = NULL)
                )
              ),
              fluidRow(
                column(6,
                  radioButtons("stmInitType", label = h5("init type"),
                    choices = list("LDA" = "LDA", "Random" = "Random", "Spectral" = "Spectral"),
                    selected = "LDA")
                )
              ),
              fluidRow(
                column(6,
                  numericInput("stmMaxEm", label = h5("max em iterations"), value = 100)
                ),
                column(6,
                  numericInput("stmEmTol", label = h5("emtol"), value = 0.00001, step = 0.00001)
                )
              ),
              fluidRow(
                column(6,
                  numericInput("stmReportEvr", label = h5("report every"), value = 1)
                )
              ),
              fluidRow(
                column(6,
                  radioButtons("stmLdaBeta", label = h5("LDA Beta"),
                    choices = list("True" = T, "False" = F), selected = T)
                ),
                column(6,
                  radioButtons("stmInteractions", label = h5("Interactions"),
                    choices = list("True" = T, "False" = F), selected = T)
                )
              ),
              fluidRow(
                column(6,
                  numericInput("stmNgroups", label = h5("ngroups"), value = 1)
                ),
                column(6,
                  radioButtons("stmGamma", label = h5("Gamma Prior"),
                    choices = list("Pooled" = "Pooled", "L1" = "L1"), selected = "Pooled")
                )
              ),
              fluidRow(
                column(6,
                  sliderInput("stmSigma", h5("Sigma Prior"),
                    min = 0, max = 1, value = 0, step = 0.01)
                ),
                column(6,
                  radioButtons("stmKappa", label = h5("Kappa Prior"),
                    choices = list("L1" = "L1", "Jeffreys" = "Jeffreys"),
                    selected = "L1")
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
                    radioButtons("plotStmType", label = h5("type"),
                      choices = list("summary" = "summary", "labels" = "labels",
                        "perspectives" = "perspectives", "hist" = "hist"), selected = "summary")
                  )
                ),
                fluidRow(
                  column(6,
                    numericInput('plotStmN', label = h5("n"), value = 3)
                  ),
                  column(6,
                    textInput('plotStmTopics',
                      label = h5("topics, comma separated (e.g. 1,2)"),
                      value = NULL)
                  )
                ),
                fluidRow(
                  column(6,
                    radioButtons("plotStmLabelType", label = h5("labeltype"),
                      choices = list("prob" = "prob", "frex" = "frex",
                        "lift" = "lift", "score" = "score"), selected = "prob")
                  ),
                  column(6,
                    numericInput("plotStmFrexw",
                      label = h5("frexw, only needed if frex is chosen for labeltype"),
                      value = 0.5, step = 0.1)
                  )
                ),
                fluidRow(
                  column(12,
                    textInput("plotStmMain", label = h5("main (title)"), value = NULL)
                  )
                ),
                fluidRow(
                  column(6,
                    textInput("plotStmXLim",
                      label = h5("x-axis limits, comma separated (e.g -.1,.1)"),
                      value = NULL)
                  ),
                  column(6,
                    textInput('plotStmYLim',
                      label = h5("y-axis limits, comma separated (e.g -10,10)"),
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
                      label = h5("topics, comma separated (e.g. 1,2)"),
                      value = NULL)
                  ),
                  column(6,
                    numericInput('labelTopicsN',
                      label = h5("n, number of words to label each topic"),
                      value = 7)
                  )
                ),
                fluidRow(
                  column(6,
                    numericInput("labelTopicsFrexw",
                      label = h5("frexweight, a weight used in FREX scoring algorithm"),
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