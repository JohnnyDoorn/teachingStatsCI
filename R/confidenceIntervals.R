#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

ConfidenceIntervals <- function(jaspResults, dataset = NULL, options) {
  
  confidenceContainer <- .getConfidenceContainer(jaspResults)
  
  .simulateDatasets(confidenceContainer, options)
  
  .computeConfidenceIntervals(confidenceContainer, options)

  .makeSummaryTable(confidenceContainer, options)
  
  .plotTreePlot(confidenceContainer, options)
  
  .makeTreeTable(confidenceContainer, options)
  
  .plotDatasets(confidenceContainer, options)
  
  return()
}

.getConfidenceContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["confidenceContainer"]])) {
    confidenceContainer <- jaspResults[["confidenceContainer"]]
  } else {
    confidenceContainer <- createJaspContainer()
    # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
    confidenceContainer$dependOn(c("mu", "sigma", "n", "confidenceIntervalInterval", "nReps", 
                                   "treePlot"))
    jaspResults[["confidenceContainer"]] <- confidenceContainer
  }
  return(confidenceContainer)
}

.simulateDatasets <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["simulatedDatasets"]]))
    return()
  
  listWithData <- lapply(1:options$nReps, function(x) rnorm(options$n,
                                                            options$mu,
                                                            options$sigma))
  
  jaspContainer[["simulatedDatasets"]] <- createJaspState(object = listWithData)
  
  return()
}

.computeConfidenceIntervals <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["computedConfidenceIntervals"]]))
    return()
  
  listWithData <- jaspContainer[["simulatedDatasets"]][["object"]]
  
  meanCI <- t(sapply(listWithData, function(x) c(mean(x), 
                                                 t.test(x, conf.level = options$confidenceIntervalInterval)$conf.int[1:2])))
  meanCI <- as.data.frame(meanCI)
  colnames(meanCI) <- c("mean", "lower", "upper")
  meanCI[["Repetition"]] <- 1:options$nReps
  meanCI[["successfulCI"]] <- options$mu > meanCI[["lower"]] & options$mu < meanCI[["upper"]]
  meanCI[["colorCI"]] <- ifelse(meanCI[["successfulCI"]], "A", "B")
  meanCI[["sd"]] <- sapply(listWithData, sd)
  meanCI[["n"]] <- sapply(listWithData, length)
  
  
  jaspContainer[["computedConfidenceIntervals"]] <- createJaspState(object = meanCI)
  
  return()
}


.makeSummaryTable <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["resultTable"]]))
    return()
  
  resultTable <- createJaspTable(title = "Results Table")
  jaspContainer[["resultTable"]] <- resultTable

  resultTable$addColumnInfo(name = "Repetitions", type = "integer")
  resultTable$addColumnInfo(name = "mu", type = "number", title=gettext("μ"))
  resultTable$addColumnInfo(name = "sigma", title=gettext("σ"), type = "number")
  resultTable$addColumnInfo(name = "n", type = "integer")
  resultTable$addColumnInfo(name = "coverage", type = "integer",title=gettext("Coverage"))
  resultTable$addColumnInfo(name = "coveragePercentage", type = "number", format = "pc", title=gettext("Coverage %"))

  meanCI <- jaspContainer[["computedConfidenceIntervals"]][["object"]]
  tableDat <- data.frame(mu = options$mu,
                         sigma = options$sigma,
                         Repetitions = options$nReps,
                         n = options$n,
                         coverage = sum(meanCI[["successfulCI"]]),
                         coveragePercentage = sum(meanCI[["successfulCI"]]) / options$nReps)
 
  resultTable$setData(tableDat)
  
  return()
}

.makeTreeTable <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["treeTable"]]) || !options$tableTreePlot)
    return()
  
  treeTable <- createJaspTable(title = "Confidence Interval Table")
  jaspContainer[["treeTable"]] <- treeTable
  
  treeTable$addColumnInfo(name = "Repetition", type = "integer")
  treeTable$addColumnInfo(name = "mean", type = "number", title=gettext("Mean"))
  treeTable$addColumnInfo(name = "n", title=gettext("Estimate"), type = "integer")
  treeTable$addColumnInfo(name = "sd", type = "number")
  
  thisOverTitle <- gettextf("%s%% CI for Mean Difference", options$confidenceIntervalInterval * 100)
  treeTable$addColumnInfo(name="lower", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
  treeTable$addColumnInfo(name="upper", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
  treeTable$addColumnInfo(name = "successfulCI", type = "string", title=gettext("CI contains μ?"))
  
  meanCI <- jaspContainer[["computedConfidenceIntervals"]][["object"]]
  meanCI[["successfulCI"]] <- ifelse(meanCI[["successfulCI"]], "Yes", "No")
  
  treeTable$showSpecifiedColumnsOnly <- TRUE
  treeTable$setData(meanCI)
  
  return()
}

.plotTreePlot <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["containerTreePlot"]]) || !options$treePlot)
    return()
  
  treePlotContainer <- createJaspContainer(title = gettext("Tree Plot"))
  jaspContainer[["containerTreePlot"]] <- treePlotContainer
  treePlotContainer$dependOn(c("treePlot", "treePlotAdditionalInfo", "fixAxisTreePlot",
                               "fixAxisLower", "fixAxisUpper"))
  treePlotContainer[["treePlot"]] <- createJaspPlot(title = "", width = 480, height = 320) 
  
  if (options$fixAxisLower >= options$fixAxisUpper) {
    treePlotContainer$setError(gettext("Make sure to specify a lower bound that is lower than the upper bound for the x-axis."))
    return()
  }
  
  
  meanCI <- jaspContainer[["computedConfidenceIntervals"]][["object"]]
  
  p <- ggplot2::ggplot(meanCI,      
                       ggplot2::aes(x = mean,
                                    y = Repetition)) +
    ggplot2::geom_point(ggplot2::aes(color = colorCI), size = 6, shape = 18) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower,
                                         xmax = upper, 
                                         color = colorCI)) +
    ggplot2::scale_color_manual(values = c("A" = "black", "B" = "red")) +
    ggplot2::geom_vline(data = data.frame(options$mu),
                        ggplot2::aes(xintercept = options$mu),
                        linetype = "dashed") +
    ggplot2::scale_y_continuous(breaks= jaspGraphs::getPrettyAxisBreaks(1:options$nReps)) +
    ggplot2::xlab(gettextf("Observed mean and %s%% CI", options$confidenceIntervalInterval * 100)) 
  
  if (options$fixAxisTreePlot) {
    p <- p + ggplot2::scale_x_continuous(limits = c(options$fixAxisLower, options$fixAxisUpper))
  }
    
  p <- p +
    jaspGraphs::geom_rangeframe() + # add lines on the x-axis and y-axis
    jaspGraphs::themeJaspRaw()      # add the JASP theme
  
  if(isTryError(p))
    treePlotContainer[["treePlot"]]$setError(.extractErrorMessage(p))
  else
    treePlotContainer[["treePlot"]]$plotObject <- p
  
  return()
}

.plotDatasets <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["containerRainCloudPlots"]]) || !options$dataPlot)
    return()
  
  rainCloudPlotsContainer <- createJaspContainer(title = gettext("Data plots"))
  # rainCloudPlotsContainer$position <- 3
  jaspContainer[["containerRainCloudPlots"]] <- rainCloudPlotsContainer
  rainCloudPlotsContainer$dependOn(c("dataPlotShowN", "dataPlot"))
  
  rainData <- data.frame(y = unlist(jaspContainer[["simulatedDatasets"]][["object"]]), 
                         group = rep(1:options$nReps, each = options$n))
  
  singlePlot <- createJaspPlot(title = "", width = 480, height = 320)
  rainCloudPlotsContainer[["rainCloudPlotAll"]] <- singlePlot
  p <- try(jaspTTests::.descriptivesPlotsRainCloudFill(rainData, "y", "group", 
                                                       yLabel = "Dependent", 
                                                       xLabel = "Rep", 
                                                       testValue = options$mu,
                                                       addLines = FALSE, horiz = FALSE))
  if(isTryError(p))
    singlePlot$setError(.extractErrorMessage(p))
  else
    singlePlot$plotObject <- p
  
  if (options$dataPlotShowN > 1) {
    for(i in 1:options$dataPlotShowN) {
      
      thisRainData <- data.frame(y = unlist(jaspContainer[["simulatedDatasets"]][["object"]][[i]]), 
                                 group = rep(1, options$n))
      
      singlePlot <- createJaspPlot(title = "", width = 480, height = 320)
      rainCloudPlotsContainer[[paste0("rainCloudPlotSingle", i)]] <- singlePlot
      p <- try(jaspTTests::.descriptivesPlotsRainCloudFill(thisRainData, "y", "group", 
                                                           yLabel = "Dependent", 
                                                           xLabel = "Rep",
                                                           testValue = options$mu,
                                                           addLines = FALSE, horiz = FALSE))
      if(isTryError(p))
        singlePlot$setError(.extractErrorMessage(p))
      else
        singlePlot$plotObject <- p
      
    }
    
  }
  
  return()
}
