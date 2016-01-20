##### stmGUI ####
#
# ui for running stm in Shiny
#

pkgs <- c("shiny", "shinyjs", "markdown", "stm", "shinyBS")
source("data_utils.R")

load.packages(pkgs)

shinyUI(tagList(
  shinyjs::useShinyjs(),
  navbarPage("stm", id = "navBar", collapsible = TRUE,
    #### DataInput ####
    tabPanel("Data",
      source(file.path("ui_files", "data_input.R"), local = TRUE)$value
    ),
    ##### Processing #####
    tabPanel("Processing",
      fluidPage(
        fluidRow(
          column(8, titlePanel("Step 2: Process and Prep Documents")),
          column(4,
            div(style = "text-align : right; padding-top : 20px;",
              actionButton(
                "moveFromProcToStm",
                "Proceed to Step 3: Generating STM Model"
              )
            )
          )
        ),
        ##### Text Processor #####
        tabsetPanel(id = "processingPanel",
          tabPanel("textProcessor",
            fluidRow(
              column(12, titlePanel("textProcessor"))
            ),
            source(file.path("ui_files", "text_processor.R"),
              local = TRUE)$value,
            ##### Plot Removed #####
            fluidRow(
              column(12, titlePanel("plotRemoved"))
            ),
            source(file.path("ui_files", "plot_removed.R"),
              local = TRUE)$value
          ),
          ##### Prep Documents #####
          tabPanel("prepDocuments",
            fluidRow(
              column(12, titlePanel("prepDocuments"))
            ),
            source(file.path("ui_files", "prep_documents.R"),
              local = TRUE)$value
          )
        )
      )
    ),
    ##### STM #####
    tabPanel("Model",
      fluidPage(
        fluidRow(
          column(12, titlePanel("Step 3: Run STM model"))
        ),
        tabsetPanel(id = "modelingPanel",
          tabPanel("stm",
            fluidRow(
              column(12, titlePanel("stm"))
              #    column(2, actionButton("exportStm", "Export STM data"))
            ),
            source(file.path("ui_files", "stm_model.R"), local = TRUE)$value
          ),
          tabPanel("estimateEffect",
            fluidRow(
              column(12, titlePanel("estimateEffect"))
            ),
            source(file.path("ui_files", "est_effect.R"),
              local = TRUE)$value
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
              source(
                file.path("ui_files", "summary_plot.R"),
                local = TRUE)$value
            ),
            tabPanel("labels",
              fluidRow(
                column(12, titlePanel("Labels Plot"))
              ),
              source(
                file.path("ui_files", "labels_plot.R"),
                local = TRUE)$value
            ),
            tabPanel("perspectives",
              fluidRow(
                column(12, titlePanel("Perspectives Plot"))
              ),
              source(
                file.path("ui_files", "perspectives_plot.R"),
                local = TRUE)$value
            ),
            tabPanel("hist",
              fluidRow(
                column(12, titlePanel("Histogram Plot"))
              ),
              source(
                file.path("ui_files", "histogram_plot.R"),
                local = TRUE)$value
            )
          )
        )
      ),
      tabPanel("plot.estimateEffect",
        fluidPage(
          fluidRow(
            column(12, titlePanel("Plot the output from estimateEffect"))
          ),
          source(
            file.path("ui_files", "estimate_eff_plot.R"),
            local = TRUE)$value
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
          source(
            file.path("ui_files", "label_topics.R"),
            local = TRUE)$value
        )
      )
    )
  )))