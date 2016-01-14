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
        tabsetPanel(id = "processingPanel",
          tabPanel("textProcessor",
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
#                 plotOutput("prPlotOutput", height = "350px"),
#                 div(style = "align : center;",
#                   sliderInput("prPlotRange", "Range:",
#                     min = 1, max = 1000, value = c(1,200), width = "100%")
#                 )
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
                  )
#                   fluidRow(
#                     column(6,
#                       tags$hr(),
#                       numericInput("plotLowThresh", label = "lower thresh", value = 1)
#                     ),
#                     column(6,
#                       tags$hr(),
#                       numericInput("plotUpThresh", label = "upper thresh", value = 200)
#                     )
#                   ),
#                   fluidRow(
#                     column(6,
#                       numericInput("plotInterval", label = "interval", value = 1)
#                     )
#                   )
                 )
              ),
              column(7,
#                verbatimTextOutput("prTextResult"),
                plotOutput("prPlotOutput", height = "350px"),
                div(style = "align : center;",
                  sliderInput("prPlotRange", "Range:",
                    min = 1, max = 1000, value = c(1,200), width = "100%")
                )
              )
            )
          ),
          ##### Prep Documents #####
          tabPanel("prepDocuments",
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
                  hr(),
                  a(id = "toggleAdvPrepDocs", "Show/hide advanced options"),
                  shinyjs::hidden(
                    div(id = "advPrepDocsOptions",
                      br(),
                      fluidRow(
                        column(6,
                          numericInput("pdLowThresh", label = "lower thresh", value = 1)
                        ),
                        column(6,
                          radioButtons("pdUpThreshChoice", label = "upper thresh",
                            choices = list("Inf" = "inf", "Manual Input:" = "manual"),
                            selected = "inf")
                        )
                      ),
                      fluidRow(
                        column(6,
                          numericInput("pdSubsample", label = "subsample", value = NULL)
                        ),
                        column(6,
                          numericInput("pdUpThresh", "", value = Inf)
                        )
                      )
                    )
                  )
                )
              ),
              column(7,
                verbatimTextOutput("pdTextResult")
              )
            )
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
        tabsetPanel(id = "modelingPanel",
          tabPanel("stm",
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
          ),
          tabPanel("estimateEffect",
            fluidRow(
              column(12, titlePanel("estimateEffect"))
            ),
            fluidRow(
              column(5,
                wellPanel(
                  fluidRow(
                    column(6, actionButton("estEffRun", "Run estimateEffect")),
                    column(6, actionButton('estEffClearout', "Clear Output"))
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
          )
        )
      )
    ),
    ##### Vizualizations #####
    navbarMenu("Plot",
      tabPanel("plot.STM",
        fluidPage(
          fluidRow(
            column(12, titlePanel("Plot an STM object"))
          ),
          tabsetPanel(id = "plotPanel",
            tabPanel("summary",
              fluidRow(
                column(12, titlePanel("Summary Plot"))
              ),
              fluidRow(
                column(5,
                  wellPanel(
                    fluidRow(
                      column(6,
                        actionButton("summaryPlotCmd", "Generate Plot!")
                      ),
                      column(6,
                        actionButton("summaryPlotClearout", "Clear Output")
                      )
                    ),
                    tags$hr(),
                    fluidRow(
                      column(6,
                        numericInput("summaryPlotN", label = "n", value = 3)
                      ),
                      column(6,
                        textInput("summaryPlotTopics",
                          label = "topics, comma separated (e.g. 1,2)",
                          value = NULL)
                      )
                    ),
                    fluidRow(
                      column(6,
                        radioButtons("summaryPlotLabelType", label = "labeltype",
                          choices = list("prob" = "prob", "frex" = "frex",
                            "lift" = "lift", "score" = "score"), selected = "prob")
                      ),
                      column(6,
                        numericInput("summaryPlotFrexw",
                          label = "frexw, only needed if frex is chosen for labeltype",
                          value = 0.5, step = 0.1)
                      )
                    ),
                    fluidRow(
                      column(12,
                        textInput("summaryPlotMain", label = "main (title)", value = NULL)
                      )
                    ),
                    fluidRow(
                      column(6,
                        textInput("summaryPlotXLim",
                          label = "x-axis limits, comma separated (e.g -.1,.1)",
                          value = "")
                      ),
                      column(6,
                        textInput("summaryPlotYLim",
                          label = "y-axis limits, comma separated (e.g -10,10)",
                          value = "")
                      )
                    )
                  )
                ),
                column(7,
                  verbatimTextOutput("summaryPlotTextResult"),
                  plotOutput("summaryPlot")
                )
              )
            ),
            tabPanel("labels",
              fluidRow(
                column(12, titlePanel("Labels Plot"))
              ),
              fluidRow(
                column(5,
                  wellPanel(
                    fluidRow(
                      column(6,
                        actionButton("labelPlotCmd", "Generate Plot!")
                      ),
                      column(6,
                        actionButton("labelPlotClearout", "Clear Output")
                      )
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
                  plotOutput("labelPlot")
                )
              )
            ),
            tabPanel("perspectives",
              fluidRow(
                column(12, titlePanel("Perspectives Plot"))
              ),
              fluidRow(
                column(5,
                  wellPanel(
                    fluidRow(
                      column(6,
                        actionButton("perspPlotCmd", "Generate Plot!")
                      ),
                      column(6,
                        actionButton("perspPlotClearout", "Clear Output")
                      )
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
            ),
            tabPanel("hist",
              fluidRow(
                column(12, titlePanel("Histogram Plot"))
              ),
              fluidRow(
                column(5,
                  wellPanel(
                    fluidRow(
                      column(6,
                        actionButton("histPlotCmd", "Generate Plot!")
                      ),
                      column(6,
                        actionButton("histPlotClearout", "Clear Output")
                      )
                    ),
                    tags$hr(),
                    fluidRow(
                      column(6,
                        numericInput("histPlotN", label = "n", value = 3)
                      ),
                      column(6,
                        textInput("histPlotTopics",
                          label = "topics, comma separated (e.g. 1,2)",
                          value = NULL)
                      )
                    ),
                    fluidRow(
                      column(6,
                        radioButtons("histPlotLabelType", label = "labeltype",
                          choices = list("prob" = "prob", "frex" = "frex",
                            "lift" = "lift", "score" = "score"), selected = "prob")
                      ),
                      column(6,
                        numericInput("histPlotFrexw",
                          label = "frexw, only needed if frex is chosen for labeltype",
                          value = 0.5, step = 0.1)
                      )
                    ),
                    fluidRow(
                      column(12,
                        textInput("histPlotMain", label = "main (title)", value = NULL)
                      )
                    ),
                    fluidRow(
                      column(6,
                        textInput("histPlotXLim",
                          label = "x-axis limits, comma separated (e.g -.1,.1)",
                          value = NULL)
                      ),
                      column(6,
                        textInput("histPlotYLim",
                          label = "y-axis limits, comma separated (e.g -10,10)",
                          value = NULL)
                      )
                    )
                  )
                ),
                column(7,
                  verbatimTextOutput("histPlotTextResult"),
                  plotOutput("histPlot")
                )
              )
            )
          )
        )
      ),
      tabPanel("plot.estimateEffect",
        fluidPage(
          fluidRow(
            column(12, titlePanel("Plot the output from estimateEffect"))
          ),
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
        )
      ),
      tabPanel("labelTopics",
        titlePanel("Generate words describing each topic from an STM object"),
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