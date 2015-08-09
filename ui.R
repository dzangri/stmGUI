##### stmGUI ####
#
# ui for running stm in Shiny
#
# author: @dzangri
#
# **TODO**: separate required params from optional ones

library(markdown)
library(stm)

shinyUI(navbarPage("stm",
  
  ##### Data Input #####
  tabPanel("Data",
    titlePanel("File Upload"),
    h4("Proceed to Processing after the data has loaded"),
    sidebarLayout(
      sidebarPanel(
        fileInput('upDatafile', 'Choose File'),
        tags$hr(),
        h6(paste("If your data is a csv or tsv, select the",
          "correct settings below, hit Submit, and a DataTable will",
          "appear on the right.")),
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
        DT::dataTableOutput('data'),
        width = 9
      )
    )
  ),
  ##### Processing #####
  tabPanel("Processing",
    titlePanel("Process and Prep Documents"),
    fluidPage(
      fluidRow(
        ##### Text Processor #####
        h3("textProcessor"),
        fluidRow(
          column(5,
            wellPanel(
              fluidRow(
                column(6,
                  actionButton('tpTextprocess', "Process Text")
                ),
                column(6,
                  actionButton('tpClearout', "Clear Output")
                )
              ),
              fluidRow(
                tags$hr(),
                column(6,
                  radioButtons("tpGadarian", label = h5("Choose textProcessor Datasource"),
                    choices = list("Gadarian Dataset" = 1, "Uploaded Data" = 2), 
                    selected = 1)
                )
              ),
              fluidRow(
                column(6,
                  textInput('tpDocs', label = h5("Name of column with text documents"))
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
              )
            )
          ),
          column(7,
            verbatimTextOutput("tpResult")
          )
        )
      ),
      ##### Plot Removed #####
      fluidRow(
        h3("plotRemoved"),
        fluidRow(
          column(5,
            wellPanel(
              fluidRow(
                column(6,
                  actionButton('prRun', "Run plotRemove")
                ),
                column(6,
                  actionButton('prClearout', "Clear Output and Plot")
                )
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
            verbatimTextOutput("prOutput"),
            plotOutput("prPlot")
          )
        )
      ),
      ##### Prep Documents #####
      # **TODO**: Support user input that is prepdoc ready?
      fluidRow(
        h3("prepDocuments"),
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
              #               fluidRow(
              #                 column(6,
              #                   radioButtons("pdChoice", label = h5("Choose data source"),
              #                     choices = list("textProcessor Output" = 1, "Raw Uploaded Data" = 2), 
              #                     selected = 1)
              #                 )
              #               ),
              #               fluidRow(
              #                 column(6,
              #                   textInput('pdDocs', label = h5("documents"))
              #                 ),
              #                 column(6,
              #                   textInput('pdVocab', label = h5("vocab"))
              #                 )
              #               ),
              #               fluidRow(
              #                 column(6,
              #                   textInput('pdMeta', label = h5("metadata"))
              #                 )
              #               ),
              fluidRow(
                tags$hr(),
                column(6,
                  numericInput("pdLowThresh", label = h5("lower thresh"), value = 1)
                ),
                # **TODO**: Is this an ok default? Option to choose Inf instead?
                #                 column(6,
                #                   numericInput("pdUpThresh", label = h5("upper thresh"), value = .Machine$integer.max)
                #                 )
                tags$hr(),
                column(6,
                  radioButtons("pdUpThreshChoice", label = h5("Upper Thresh"),
                    choices = list("Inf" = 1, "Manual Input" = 2), 
                    selected = 1),
                  numericInput("pdUpThresh", label = h5(""), value = 10000)
                )
              ),
              fluidRow(
                column(6,
                  numericInput("pdSubsample", label = h5("subsample"), value = NULL)
                ),
                column(6,
                  radioButtons("pdVerbose", label = h5("verbose"),
                    choices = list("True" = T, "False" = F), selected = T)
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
  ##### STM #####
  # **TODO**: Find sane limits for numericInputs
  tabPanel("Model",
    titlePanel("Enter Parameters and Run STM model"),
    fluidPage(
      fluidRow(
        column(10,
          h3("stm")
        ),
        column(2,
          actionButton('exportStm', "Export STM data")
        )
      ),
      fluidRow(
        column(5,
          wellPanel(
            #               fluidRow(
            #                 column(6,
            #                   radioButtons("stmChoice", label = h5("Choose data source"),
            #                     choices = list("prepDocuments Output" = 1, "Uploaded Data" = 2), 
            #                     selected = 1)
            #                 )
            #               ),
            #               fluidRow(
            #                 column(6,
            #                   textInput('stmDocs', label = h5("documents"))
            #                 ),
            #                 column(6,
            #                   textInput('stmVocab', label = h5("vocab"))
            #                 )
            #               ),
            fluidRow(
              column(6,
                numericInput("stmK", label = h5("K"), value = 3)
              ),
              column(6,
                textInput('stmPrev', label = h5("prevalence formula, include leading ~"), value = NULL)
              )
            ),
            fluidRow(
              column(6,
                textInput('stmContent', label = h5("content"), value = NULL)
              ),
              column(6,
                numericInput("stmSeed", label = h5("seed"), value = 94301)
              )
            ),
            fluidRow(
              column(6,
                radioButtons("stmInitType", label = h5("init type"),
                  choices = list("LDA" = "LDA", "Random" = "Random", "Spectral" = "Spectral"), selected = "LDA")
              )
            ),
            fluidRow(
              column(6,
                numericInput("stmMaxEm", label = h5("max em iterations"), value = 10)
              ),
              column(6,
                numericInput('stmEmTol', label = h5("emtol"), value = 0.00001, step = 0.00001)
              )
            ),
            fluidRow(
              column(6,
                radioButtons("stmVerbose", label = h5("verbose"),
                  choices = list("True" = T, "False" = F), selected = T)
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
#     tabPanel("findThoughts",
#       "")
  )
))