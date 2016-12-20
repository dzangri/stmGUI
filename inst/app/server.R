##### stmGUI ####
# Server-side logic for handling stm workflows

options(shiny.maxRequestSize = 100 * 1024^2)

## util functions
observeNextStep <- function(input, output, session) {
  reactNextStep <- reactive({
    input$nextStep
  })
  return(reactNextStep)
}

observeClearout <- function(input, output, session) {
  reactClearout <- reactive({
    input$clearout
  })
  return(reactClearout)
}

changeCsStringToDoubleVectorOrLeaveNull <- function(cs.string) {
  string.to.vector <- cs.string
  if (nchar(string.to.vector) > 0) {
    string.as.double <- c(as.double(strsplit(string.to.vector, ",")[[1]]))
    return(string.as.double)
  } else {
    return(NULL)
  }
}

changeCsStringToVectorOrLeaveNull <- function(cs.string) {
  string.to.vector <- cs.string
  if (nchar(string.to.vector) > 0) {
    string.as.string.vec <- c(strsplit(string.to.vector, ",")[[1]])
    return(string.as.string.vec)
  } else {
    return(NULL)
  }
}

changeEmptyStringToNull <- function(possiblyEmptyString) {
  if (possiblyEmptyString == "") {
    return(NULL)
  } else {
    return(possiblyEmptyString)
  }
}

changeStringToLogical <- function(logicalString) {
  if (logicalString) {
    return(T)
  } else {
    return(F)
  }
}

changeStringNullToNull <- function(nullStr) {
  if (nullStr == "NULL") {
    return(NULL)
  }
  return(nullStr)
}

errorForUi <- function(errorMessage) {
  return(paste("ERROR:", errorMessage))
}

## shiny server
shinyServer(function(input, output, session) {

  #### TEST CODE HERE #### load('stmResult.RData')

  # reactive object that stores intermediate results
  storedData <- reactiveValues()

  storedData$data <- NULL
  storedData$textprocess <- NULL
  storedData$prepdocs <- NULL
  storedData$stmresult <- NULL
  storedData$stmformula <- NULL
  storedData$esteffect <- NULL

  storedData$summaryPlotArgs <- NULL
  storedData$labelsPlotArgs <- NULL
  storedData$perspectivesPlotArgs <- NULL
  storedData$histPlotArgs <- NULL
  storedData$estEffPlotArgs <- NULL

  dataInputTitleObserver <- callModule(observeNextStep, "dataInputTitle")
  processingTitleObserver <- callModule(observeNextStep, "processingTitle")
  stmTitleObserver <- callModule(observeNextStep, "stmTitle")

  observeEvent(dataInputTitleObserver(), ({
    updateTabsetPanel(session, "navBar", selected = "Processing")
  }))
  observeEvent(processingTitleObserver(), ({
    updateTabsetPanel(session, "navBar", selected = "Model")
  }))
  observeEvent(stmTitleObserver(), ({
    updateTabsetPanel(session, "navBar", selected = "plot.STM")
  }))

  tpClearoutObserver <- callModule(observeClearout, "tpClearout")
  prClearoutObserver <- callModule(observeClearout, "prClearout")
  pdClearoutObserver <- callModule(observeClearout, "pdClearout")
  stmClearoutObserver <- callModule(observeClearout, "stmClearout")
  estEffClearoutObserver <- callModule(observeClearout, "estEffClearout")
  summaryPlotClearoutObserver <- callModule(observeClearout, "summaryPlotClearout")
  labelsPlotClearoutObserver <- callModule(observeClearout, "labelsPlotClearout")
  perspPlotClearoutObserver <- callModule(observeClearout, "perspPlotClearout")
  histPlotClearoutObserver <- callModule(observeClearout, "histPlotClearout")
  estEffPlotClearoutObserver <- callModule(observeClearout, "estEffPlotClearout")
  labelTopicsClearoutObserver <- callModule(observeClearout, "labelTopicsClearout")

  observeEvent(tpClearoutObserver(), ({
    shinyjs::html("tpTextResult", "")
  }))
  observeEvent(prClearoutObserver(), ({
    shinyjs::html("prPlotOutput", "")
  }))
  observeEvent(pdClearoutObserver(), ({
    shinyjs::html("pdTextResult", "")
  }))
  observeEvent(stmClearoutObserver(), ({
    shinyjs::html("stmTextResult", "")
  }))
  observeEvent(estEffClearoutObserver(), ({
    shinyjs::html("estEffTextResult", "")
  }))
  observeEvent(summaryPlotClearoutObserver(), ({
    shinyjs::html("summaryPlotTextResult", "")
    shinyjs::html("summaryPlot", "")
  }))
  observeEvent(labelsPlotClearoutObserver(), ({
    shinyjs::html("labelPlotTextResult", "")
    shinyjs::html("labelPlot", "")
  }))
  observeEvent(perspPlotClearoutObserver(), ({
    shinyjs::html("perspPlotTextResult", "")
    shinyjs::html("perspPlot", "")
  }))
  observeEvent(histPlotClearoutObserver(), ({
    shinyjs::html("histPlotTextResult", "")
    shinyjs::html("histPlot", "")
  }))
  observeEvent(estEffPlotClearoutObserver(), ({
    shinyjs::html("estEffPlotTextResult", "")
    shinyjs::html("estEffPlot", "")
  }))
  observeEvent(labelTopicsClearoutObserver(), ({
    shinyjs::html("labelTopicsTextResult", "")
    shinyjs::html("labelTopicsPlot", "")
  }))

  ##### Data Upload ##### Allow file upload and submission to be read by read.csv as the raw data in shiny
  ##### **TODO**: Expand file types that can be handled
  shinyjs::onclick("toggleAdvDataUpload",
                   shinyjs::toggle(id = "advUploadOptions",
                          anim = TRUE))
  observe({
    shinyjs::toggleState("submitDataForUpload",
                         !is.null(input$dataFileToUpload))
  })
  observe({
    shinyjs::toggleState("dataInputTitle-nextStep",
                         !is.null(storedData$data))
  })

  observeEvent(input$submitDataForUpload, ({
    shinyjs::html("dataInputTextResult", "")

    userData <- input$dataFileToUpload

    withProgress(message = "Loading data, please wait...", {
      setProgress(0.5)

      readDataArgs <- list(userData$datapath, header = input$headerPresent, sep = input$columnSeparator,
                           quote = input$quoteAroundData)

      shinyjs::toggleState("moveFromStep1To2")

      tryCatch({
        storedData$data <- do.call(read.csv, readDataArgs)
      }, error = function(e) {
        funName <- deparse(substitute(read.csv))
        shinyjs::html("dataInputTextResult",
                      paste("ERROR: Error while running '",
                            funName, "':\n",
                            e,
                            sep = ""))
        storedData$data <- NULL
        return(NULL)
      }, warning = function(w) {
        shinyjs::html("dataInputTextResult",
                      paste("WARNING: Warning while reading data:\n",
                            w,
                            sep = "\n"))
        storedData$data <- NULL
        return(NULL)
      }, finally = {
      })

      setProgress(1)
    })
    output$data <- DT::renderDataTable({
      DT::datatable(storedData$data)
    })
  }))

  ##### Text Processor ##### processes input to textProcessor includes option to choose data from preloaded
  ##### gadarian data or data uploaded by the user
  shinyjs::onclick("toggleAdvTextProc",
                   shinyjs::toggle(id = "advTextProcOptions",
                          anim = TRUE))
  observe({
    shinyjs::toggleState("processingTitle-nextStep",
                         !is.null(storedData$prepdocs))
  })

  # Only activate the submit button if either the preloaded gadarian data is chosen OR the user has uploaded
  # data
  observe({
    shinyjs::toggleState("tpProcessText",
                         input$tpGadarian == "gadar" || !is.null(storedData$data))
  })

  # Update document options based on column names of uploaded data
  observe({
    userData <- storedData$data
    if (!is.null(userData)) {
      shinyjs::enable("tpDocs")
      dataColumnNames <- colnames(userData)
      updateSelectInput(session, "tpDocs", choices = dataColumnNames)
    } else {
      shinyjs::disable("tpDocs")
    }
  })

  observe({
    if (is.null(storedData$data)) {
      updateRadioButtons(session,
                         "tpGadarian",
                         choices = list(`Preloaded Gadarian Dataset` = "gadar",
                                        `Uploaded Data` = "upload"),
                         selected = "gadar")
    } else {
      updateRadioButtons(session,
                         "tpGadarian",
                         choices = list(`Preloaded Gadarian Dataset` = "gadar",
                                        `Uploaded Data` = "upload"),
                         selected = "upload")
    }
  })

  observeEvent(input$tpProcessText, ({

    docsForTp <- NULL
    metadataForTp <- NULL
    dataType <- NULL

    if (input$tpGadarian == "gadar") {
      docsForTp <- gadarian$open.ended.response
      metadataForTp <- gadarian
      dataType <- "Gadarian"
    } else {
      docs <- input$tpDocs
      docsForTp <- storedData$data[[docs]]
      metadataForTp <- storedData$data
      dataType <- "user"
    }

    if (!is.null(docsForTp)) {
      # **TODO**: Implement wordLengths, customstopwords, onlytxtfiles
      shinyjs::html("tpTextResult", "textProcessor is running...")

      argsForTp <- list(documents = docsForTp, metadata = metadataForTp, lowercase = input$tpLowercase,
                        removestopwords = input$tpRemovestop, removenumbers = input$tpRemovenum, removepunctuation = input$tpRemovepunc,
                        stem = input$tpStem, wordLengths = c(3, Inf), sparselevel = input$tpSparselevel, language = input$tpLang,
                        onlycharacter = input$tpOnlychar, striphtml = input$tpStriphtml, customstopwords = NULL, onlytxtfiles = TRUE)

      withProgress(message = "Running textProcessor, please wait...", {
        setProgress(0.5)
        tpOutputRaw <- tryCatch({
          paste(capture.output(isolate(storedData$textprocess <- do.call(textProcessor, argsForTp))),
                collapse = "\n")
        }, error = function(e) {
          errorMessage <- paste(errorForUi("Error while running textProcessor:"), e, sep = "\n")
          storedData$textprocess <- NULL
          return(errorMessage)
        }, warning = function(w) {
          warningMessage <- paste("WARNING: Warning while running textProcessor:", w, sep = "\n")
          storedData$textprocess <- NULL
          return(warningMessage)
        }, finally = {
          setProgress(1)
        })
      })
      if (!is.null(storedData$textprocess)) {
        if (length(storedData$textprocess$vocab) > 0) {
          doneAlert <- sprintf("Done processing %s data!", dataType)
          completeMessage <- paste(tpOutputRaw, doneAlert, sep = "\n")
          shinyjs::html("tpTextResult", completeMessage)
          vocabLength = length(storedData$textprocess$vocab)
          newMax = as.integer(vocabLength + 0.1 * vocabLength)

          updateSliderInput(session, "prPlotRange", "Lower threshold values to test word and document removal from the sample:",
                            min = 1, max = newMax, value = c(1, newMax/2))
        } else {
          shinyjs::html("tpTextResult", errorForUi(paste("textProcessor did not complete correctly! This occurred because",
                                                         "the output of textProcessor contained a vocab size of 0! Perhaps you entered a", "nontextual data column as the choice for documents?")))
          storedData$textprocess <- NULL
        }
      } else {
        shinyjs::html("tpTextResult", tpOutputRaw)
      }
    } else {
      shinyjs::html("tpTextResult", errorForUi("Try again with column name of the document vector!"))
    }
  }))

  ##### Plot Removed #####
  observe({
    shinyjs::toggleState("prRun", !is.null(storedData$textprocess))
  })

  observeEvent(input$prRun, ({
    if (is.null(storedData$textprocess)) {
      return(NULL)
    }

    plotRange <- as.integer(input$prPlotRange)

    min.thresh.element <- plotRange[[1]]
    max.thresh.element <- plotRange[[2]]
    jump <- 1
    num.in.lower.thresh <- (max.thresh.element - min.thresh.element)/jump

    while (num.in.lower.thresh > 2000) {
      jump <- jump * 2
      num.in.lower.thresh <- (max.thresh.element - min.thresh.element)/jump
    }

    output$prPlotOutput <- renderPlot(isolate(plotRemoved(storedData$textprocess$documents, lower.thresh = seq(from = min.thresh.element,
                                                                                                               to = max.thresh.element, by = jump))))
  }))

  ##### Prep Documents ##### calculates on the output from text processor or calculates on the file uploaded by
  ##### the user **TODO**: expand functionality for case of user-input file
  shinyjs::onclick("toggleAdvPrepDocs",
                   shinyjs::toggle(id = "advPrepDocsOptions", anim = TRUE))

  observe({
    shinyjs::toggleState("pdPrepdocs", !is.null(storedData$textprocess))
  })

  # Change Upper Thresh
  observe({
    pdUpThreshType <- input$pdUpThreshChoice

    shinyjs::toggleState("pdUpThresh", pdUpThreshType == "manual")

    if (pdUpThreshType == "inf") {
      updateNumericInput(session, "pdUpThresh", value = Inf)
    } else {
      updateNumericInput(session, "pdUpThresh", value = 10000)
    }
  })

  observeEvent(input$pdClearout, ({
    shinyjs::html("pdTextResult", "")
  }))

  observeEvent(input$pdPrepdocs, ({

    if (is.null(storedData$textprocess)) {
      shinyjs::html("pdTextResult", "You must successfully run textProcessor!")
      return(NULL)
    }

    upperThresh <- Inf
    if (input$pdUpThreshChoice == "manual") {
      upperThresh <- input$pdUpThresh
    }

    tpres <- storedData$textprocess

    shinyjs::html("pdTextResult", "Running Prep Documents...")
    withProgress(message = "Running prepDocuments, please wait...", {
      setProgress(0.5)

      argsForPd <- list(documents = tpres$documents, vocab = tpres$vocab, meta = tpres$meta, lower.thresh = input$pdLowThresh,
                        upper.thresh = upperThresh, subsample = NULL)

      pdOutputRaw <- tryCatch({
        paste(capture.output(isolate(storedData$prepdocs <- do.call(prepDocuments, argsForPd))), collapse = "\n")
      }, error = function(e) {
        errorMessage <- paste(errorForUi("Error while running prepDocuments:"), e, sep = "\n")
        storedData$prepdocs <- NULL
        return(errorMessage)
      }, warning = function(w) {
        warningMessage <- paste("WARNING: Warning while running prepDocuments:", w, sep = "\n")
        storedData$prepdocs <- NULL
        return(warningMessage)
      }, finally = {
        setProgress(1)
      })
    })
    shinyjs::html("pdTextResult", pdOutputRaw)
  }))

  ##### STM #####
  shinyjs::onclick("toggleAdvStm", shinyjs::toggle(id = "advStmOptions", anim = TRUE))

  observe({
    shinyjs::toggleState("stmRun", !is.null(storedData$prepdocs))
  })

  observe({
    shinyjs::toggleState("stmTitle-nextStep", !is.null(storedData$stmresult))
  })

  observe({
    if (!is.null(storedData$textprocess)) {
      if (length(storedData$textprocess$vocab) < 10000) {
        updateRadioButtons(session,
                           "stmInitType",
                           label = "init type",
                           choices = list(LDA = "LDA",
                                          Random = "Random",
                                          Spectral = "Spectral"),
                           inline = T,
                           selected = "Spectral")
      }
    }
  })

  observeEvent(input$stmRun, ({

    pdres <- storedData$prepdocs
    if (is.null(pdres)) {
      shinyjs::html("stmTextResult", "You must successfully run Prep Documents!")
      return(NULL)
    }

    stmArgs <- list()

    if (is.null(input$stmK)) {
      shinyjs::html("stmTextResult", "K cannot be left blank!")
      return(NULL)
    }

    withProgress(message = "Generating STM model, please wait...", {
      setProgress(0.5)

      stmArgs <- list(documents = pdres$documents,
                      vocab = pdres$vocab,
                      K = input$stmK,
                      data = pdres$meta,
                      init.type = input$stmInitType,
                      max.em.its = input$stmMaxEm,
                      emtol = input$stmEmTol,
                      reportevery = input$stmReportEvr,
                      LDAbeta = changeStringToLogical(input$stmLdaBeta),
                      interactions =
                        changeStringToLogical(input$stmInteractions),
                      ngroups = input$stmNgroups,
                      gamma.prior = input$stmGamma,
                      sigma.prior = input$stmSigma,
                      kappa.prior = input$stmKappa)

      prevString <- input$stmPrev
      if (prevString != "") {
        try(stmArgs <- c(stmArgs, prevalence = formula(prevString)))
      }

      inputSeed <- input$stmSeed
      if (!is.na(inputSeed)) {
        try(stmArgs <- c(stmArgs, seed = inputSeed))
      }

      contentString <- input$stmContent
      if (contentString != "") {
        try(stmArgs <- c(stmArgs, content = formula(contentString)))
      }

      stmOutputRaw <- tryCatch({
        paste(capture.output(isolate(storedData$stmresult <- do.call(stm, stmArgs))), collapse = "\n")
      }, error = function(e) {
        errorMessage <- paste(errorForUi("Error while running stm:"), e, sep = "\n")
        storedData$stmresult <- NULL
        return(errorMessage)
      }, warning = function(w) {
        warningMessage <- paste("WARNING: Warning while running stm:", w, sep = "\n")
        storedData$stmresult <- NULL
        return(warningMessage)
      }, finally = {
        setProgress(1)
      })
      storedData$stmformula <- prevString
    })
    shinyjs::html("stmTextResult", stmOutputRaw)
  }))

  # observeEvent(input$exportStm, ({ stmObj <- storedData$stmresult if (is.null(storedData)) {
  # shinyjs::html('stmTextResult', 'You must successfully run STM before saving!') return(NULL) }
  # save(storedData, file='stmResult.RData') }))

  ##### Estimate Effect #####
  shinyjs::onclick("toggleAdvEstEff",
                   shinyjs::toggle(id = "advEstEffOptions", anim = TRUE))

  observe({
    shinyjs::toggleState("estEffRun", !is.null(storedData$stmresult))
  })

  observe({
    stmFormula <- storedData$stmformula
    if (!is.null(storedData$stmresult) && !is.null(stmFormula)) {
      updateTextInput(session, "estEffFormula", "formula\n(include leading ~)", stmFormula)
    }
  })

  observeEvent(input$estEffClearout, ({
    shinyjs::html("estEffTextResult", "")
  }))

  observeEvent(input$estEffRun, ({
    stmObj <- storedData$stmresult

    if (is.null(stmObj)) {
      shinyjs::html("estEffTextResult", "You must successfully run STM before running estimateEffect!")
      return(NULL)
    }

    estEffArgs <- list(stmobj = stmObj, metadata = storedData$prepdocs$meta, uncertainty = input$estEffUncertainty,
                       nsims = input$estEffNSims)
    formParseOutput <- ""

    estEffString <- input$estEffFormula
    if (estEffString != "") {
      try(estEffArgs <- c(estEffArgs, formula = formula(estEffString)))
    } else {
      shinyjs::html("estEffTextResult", "You must enter a formula to run estimateEffect!")
      return(NULL)
    }

    if (input$estEffUncertainty == "Local") {
      try(estEffArgs <- c(estEffArgs, documents = storedData$prepdocs$documents))
    }

    prior <- input$estEffPrior
    if (prior != "") {
      try(estEffArgs <- c(estEffArgs, prior = changeCsStringToDoubleVectorOrLeaveNull(prior)))
    }

    withProgress(message = "Running estimateEffect, please wait...", {
      setProgress(0.5)

      estEffOutputRaw <- tryCatch({
        paste(capture.output(isolate(storedData$esteffect <- do.call(estimateEffect, estEffArgs))),
              collapse = "\n")
      }, error = function(e) {
        errorMessage <- paste(errorForUi("Error while running estimateEffect:"), e, sep = "\n")
        storedData$esteffect <- NULL
        return(errorMessage)
      }, warning = function(w) {
        warningMessage <- paste("WARNING: Warning while running estimateEffect:", w, sep = "\n")
        storedData$esteffect <- NULL
        return(warningMessage)
      }, finally = {
        setProgress(1)
      })
    })
    shinyjs::html("estEffTextResult", "Ran estimateEffect!")
  }))

  ##### Plot Estimate Effect #####
  observeEvent(input$doEstEffPlot, ({
    estEff <- storedData$esteffect
    stmObj <- storedData$stmresult

    if (is.null(estEff)) {
      shinyjs::html("estEffPlotTextResult", "You must successfully run estimateEffect before plotting!")
      return(NULL)
    }

    plotMethod <- input$estEffPlotMethod

    tops <- changeCsStringToDoubleVectorOrLeaveNull(input$estEffPlotTopics)
    if (is.null(tops)) {
      tops <- estEff$topics
    }

    estEffXLim <- changeCsStringToDoubleVectorOrLeaveNull(input$estEffXLim)
    estEffYLim <- changeCsStringToDoubleVectorOrLeaveNull(input$estEffYLim)

    storedData$estEffPlotArgs <- list(x = estEff, covariate = input$estEffPlotCov, model = stmObj, topics = tops,
                                      method = plotMethod, xlim = estEffXLim, ylim = estEffYLim)

    if (plotMethod == "difference") {
      covVar1 <- input$estEffCovVar1
      covVar2 <- input$estEffCovVar2
      try(storedData$estEffPlotArgs <- c(storedData$estEffPlotArgs, cov.value1 = covVar1, cov.value2 = covVar2))
    }

    estEffModerator <- changeEmptyStringToNull(input$estEffMod)
    estEffModeratorVal <- changeEmptyStringToNull(input$estEffModValue)
    if (!is.null(estEffModerator)) {
      try(storedData$estEffPlotArgs <- c(storedData$estEffPlotArgs, moderator = estEffModerator))
    }
    if (!is.null(estEffModeratorVal)) {
      try(storedData$estEffPlotArgs <- c(storedData$estEffPlotArgs, moderator.value = estEffModeratorVal))
    }

    if (plotMethod == "continuous") {
      npoints <- changeEmptyStringToNull(input$estEffNPoints)
      lineCols <- changeCsStringToVectorOrLeaveNull(input$estEffLineCol)
      if (!is.null(npoints)) {
        try(storedData$estEffPlotArgs <- c(storedData$estEffPlotArgs, npoints = npoints))
      }
      if (!is.null(lineCols)) {
        try(storedData$estEffPlotArgs <- c(storedData$estEffPlotArgs, linecol = lineCols))
      }
    }

    output$estEffPlot <- renderPlot({
      isolate(do.call(plot.estimateEffect, storedData$estEffPlotArgs))
    })
  }))

  output$estEffPlotDownload <- downloadHandler(filename = function() {
    paste("estEffPlot", Sys.Date(), ".pdf", sep = "")
  }, content = function(file) {
    pdf(file)
    do.call(plot.estimateEffect, storedData$estEffPlotArgs)
    dev.off()
  })

  observe({
    shinyjs::toggleState("estEffPlotDownload", !is.null(storedData$estEffPlotArgs))
  })

  ##### plot STM #####
  ####$ currently leaving out as inputs until future input: family, width, covarlevels, plabels,
  ##### text.cex custom.labels, topic.names

  observeEvent(input$summaryPlotClearout, ({
    shinyjs::html("summaryPlotTextResult", "")
    output$summaryPlot <- renderPlot({
      invisible(NULL)
    })
  }))

  observe({
    shinyjs::toggleState("summaryPlotDownload", !is.null(storedData$summaryPlotArgs))
  })

  observe({
    shinyjs::toggleState("labelsPlotDownload", !is.null(storedData$labelsPlotArgs))
  })

  observe({
    shinyjs::toggleState("perspectivesPlotDownload", !is.null(storedData$perspectivesPlotArgs))
  })

  observe({
    shinyjs::toggleState("histPlotDownload", !is.null(storedData$histPlotArgs))
  })

  observeEvent(input$summaryPlotCmd, ({
    stmObj <- storedData$stmresult

    if (is.null(stmObj)) {
      shinyjs::html("summaryPlotTextResult", "You must successfully run STM before plotting!")
      return(NULL)
    }

    tops <- changeCsStringToDoubleVectorOrLeaveNull(input$summaryPlotTopics)
    plotXLim <- changeCsStringToDoubleVectorOrLeaveNull(input$summaryPlotXLim)
    plotYLim <- changeCsStringToDoubleVectorOrLeaveNull(input$summaryPlotYLim)

    storedData$summaryPlotArgs <- list(x = stmObj, type = "summary", n = input$summaryPlotN, topics = tops,
                                       labeltype = input$summaryPlotLabelType, frexw = input$summaryPlotFrexw, main = input$summaryPlotMain,
                                       xlim = plotXLim, ylim = plotYLim)

    output$summaryPlot <- renderPlot({
      isolate(do.call(plot.STM, storedData$summaryPlotArgs))
    })
  }))

  output$summaryPlotDownload <- downloadHandler(filename = function() {
    paste("stmSummaryPlot", Sys.Date(), ".pdf", sep = "")
  }, content = function(file) {
    pdf(file)
    do.call(plot.STM, storedData$summaryPlotArgs)
    dev.off()
  })

  observeEvent(input$labelPlotCmd, ({
    stmObj <- storedData$stmresult

    if (is.null(stmObj)) {
      shinyjs::html("labelPlotTextResult", "You must successfully run STM before plotting!")
      return(NULL)
    }

    tops <- changeCsStringToDoubleVectorOrLeaveNull(input$labelPlotTopics)
    plotXLim <- changeCsStringToDoubleVectorOrLeaveNull(input$labelPlotXLim)
    plotYLim <- changeCsStringToDoubleVectorOrLeaveNull(input$labelPlotYLim)

    storedData$labelsPlotArgs <- list(x = stmObj, type = "labels", n = input$labelPlotN, topics = tops,
                                      labeltype = input$labelPlotLabelType, frexw = input$labelPlotFrexw, main = input$labelPlotMain,
                                      xlim = plotXLim, ylim = plotYLim)

    output$labelPlot <- renderPlot({
      isolate(do.call(plot.STM, storedData$labelsPlotArgs))
    })
  }))

  output$labelsPlotDownload <- downloadHandler(filename = function() {
    paste("stmLabelsPlot", Sys.Date(), ".pdf", sep = "")
  }, content = function(file) {
    pdf(file)
    do.call(plot.STM, storedData$labelsPlotArgs)
    dev.off()
  })

  observeEvent(input$perspPlotCmd, ({
    stmObj <- storedData$stmresult

    if (is.null(stmObj)) {
      shinyjs::html("perspPlotTextResult", "You must successfully run STM before plotting!")
      return(NULL)
    }

    tops <- changeCsStringToDoubleVectorOrLeaveNull(input$perspPlotTopics)
    plotXLim <- changeCsStringToDoubleVectorOrLeaveNull(input$perspPlotXLim)
    plotYLim <- changeCsStringToDoubleVectorOrLeaveNull(input$perspPlotYLim)

    storedData$perspectivesPlotArgs <- list(x = stmObj, type = "perspectives", n = input$perspPlotN, topics = tops,
                                            labeltype = input$perspPlotLabelType, frexw = input$perspPlotFrexw, main = input$perspPlotMain,
                                            xlim = plotXLim, ylim = plotYLim)

    output$perspPlot <- renderPlot({
      isolate(do.call(plot.STM, storedData$perspectivesPlotArgs))
    })
  }))

  output$perspectivesPlotDownload <- downloadHandler(filename = function() {
    paste("stmPerspectivesPlot", Sys.Date(), ".pdf", sep = "")
  }, content = function(file) {
    pdf(file)
    do.call(plot.STM, storedData$perspectivesPlotArgs)
    dev.off()
  })

  observeEvent(input$histPlotCmd, ({
    stmObj <- storedData$stmresult

    if (is.null(stmObj)) {
      shinyjs::html("histPlotTextResult", "You must successfully run STM before plotting!")
      return(NULL)
    }

    tops <- changeCsStringToDoubleVectorOrLeaveNull(input$histPlotTopics)
    plotXLim <- changeCsStringToDoubleVectorOrLeaveNull(input$histPlotXLim)
    plotYLim <- changeCsStringToDoubleVectorOrLeaveNull(input$histPlotYLim)

    storedData$histPlotArgs <- list(x = stmObj, type = "hist", n = input$histPlotN, topics = tops, labeltype = input$histPlotLabelType,
                                    frexw = input$histPlotFrexw, main = input$histPlotMain, xlim = plotXLim, ylim = plotYLim)

    output$histPlot <- renderPlot({
      isolate(do.call(plot.STM, storedData$histPlotArgs))
    })
  }))

  output$histPlotDownload <- downloadHandler(filename = function() {
    paste("stmHistPlot", Sys.Date(), ".pdf", sep = "")
  }, content = function(file) {
    pdf(file)
    do.call(plot.STM, storedData$histPlotArgs)
    dev.off()
  })

  observeEvent(input$labelTopics, ({
    stmObj <- storedData$stmresult

    if (is.null(stmObj)) {
      shinyjs::html("labelTopicsTextResult", "You must successfully run STM before running Label Topics")
      return(NULL)
    }

    tops <- changeCsStringToDoubleVectorOrLeaveNull(input$labelTopicsTopics)

    output$labelTopicsTextResult <- renderPrint({
      isolate(labelTopics(stmObj, n = input$labelTopicsN, topics = tops, frexweight = input$labelTopicsFrexw))
    })
  }))
})
