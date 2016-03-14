##### stmGUI ####
#
# ui for running stm in Shiny
#

source("data_utils.R")
source("ui_utils.R")

pkgs <- c("shiny", "shinyjs", "markdown", "stm", "shinyBS", "shinythemes")
load.packages(pkgs)



shinyUI(tagList(
  includeCSS("www/flatly.css"),
  includeCSS("www/style.css"),
  shinyjs::useShinyjs(),
  navbarPage("stm",
    id = "navBar",
    collapsible = TRUE,
    theme = shinytheme("cosmo"),
    #### DataInput ####
    tabPanel("Data",
      source(file.path("ui_files", "data_input.R"), local = TRUE)$value
    ),
    ##### Processing #####
    tabPanel("Processing",
      fluidPage(
        fluidRow(
          titleWithNextStep(
            "processingTitle",
            "Step 2: Process & Prep Documents",
            "Proceed to Step 3: Generating STM Model"
          )
        ),
        ##### Text Processor #####
        tabsetPanel(id = "processingPanel",
          tabPanel("textProcessor",
            fluidRow(
              titleWithClearout("tpClearout", "textProcessor")
            ),
            source(file.path("ui_files", "text_processor.R"),
              local = TRUE)$value,
            ##### Plot Removed #####
            fluidRow(
              titleWithClearout("prClearout", "plotRemoved")
            ),
            source(file.path("ui_files", "plot_removed.R"),
              local = TRUE)$value
          ),
          ##### Prep Documents #####
          tabPanel("prepDocuments",
            fluidRow(
              titleWithClearout("pdClearout", "prepDocuments")
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
          titleWithNextStep(
            "stmTitle",
            "Step 3: Run STM model",
            "Proceed to Plotting"
          )
        ),
        tabsetPanel(id = "modelingPanel",
          tabPanel("stm",
            fluidRow(
              titleWithClearout("stmClearout", "stm")
            ),
            source(file.path("ui_files", "stm_model.R"), local = TRUE)$value
          ),
          tabPanel("estimateEffect",
            fluidRow(
              titleWithClearout("estEffClearout", "estimateEffect")
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
                titleWithClearout("summaryPlotClearout", "Summary Plot")
              ),
              source(
                file.path("ui_files", "summary_plot.R"),
                local = TRUE)$value
            ),
            tabPanel("labels",
              fluidRow(
                titleWithClearout("labelsPlotClearout", "Labels Plot")
              ),
              source(
                file.path("ui_files", "labels_plot.R"),
                local = TRUE)$value
            ),
            tabPanel("perspectives",
              fluidRow(
                titleWithClearout("perspPlotClearout", "Perspectives Plot")
              ),
              source(
                file.path("ui_files", "perspectives_plot.R"),
                local = TRUE)$value
            ),
            tabPanel("hist",
              fluidRow(
                titleWithClearout("histPlotClearout", "Histogram Plot")
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
            titleWithClearout("estEffPlotClearout", "Estimate Effect Plot")
          ),
          source(
            file.path("ui_files", "estimate_eff_plot.R"),
            local = TRUE)$value
        )
      ),
      tabPanel("labelTopics",
        fluidPage(
          fluidRow(
            column(12,
              titlePanel(
                "Generate words describing each topic from an STM object"
              )
            )
          ),
          fluidRow(
            titleWithClearout("labelTopicsClearout", "labelTopics")
          ),
          source(
            file.path("ui_files", "label_topics.R"),
            local = TRUE)$value
        )
      )
    )
  )))