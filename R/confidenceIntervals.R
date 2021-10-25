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
  
  .plotTreePlot(confidenceContainer, options)
  
  .plotDatasets(confidenceContainer, options)
  
  # browser()
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

.plotTreePlot <- function(jaspContainer, options) {
  if (!is.null(jaspContainer[["containerTreePlot"]]) || !options$treePlot)
    return()
  
  treePlotContainer <- createJaspContainer(title = gettext("Data plots"))
  jaspContainer[["containerTreePlot"]] <- treePlotContainer
  treePlotContainer$dependOn(c("treePlot", "treePlotAdditionalInfo"))
  
  listWithData <- jaspContainer[["simulatedDatasets"]][["object"]]
  meanCI <- t(sapply(listWithData, function(x) c(mean(x), 
                                                 t.test(x, conf.level = options$confidenceIntervalInterval)$conf.int[1:2])))
  
  meanCI <- as.data.frame(meanCI)
  colnames(meanCI) <- c("mean", "lower", "upper")
  meanCI[["reps"]] <- 1:options$nReps
  meanCI[["successfulCI"]] <- options$mu > meanCI[["lower"]] & options$mu < meanCI[["upper"]]
  meanCI[["colorCI"]] <- ifelse(meanCI[["successfulCI"]], "A", "B")
  
  p <- ggplot2::ggplot(meanCI,        # Create default ggplot2 scatterplot
                       ggplot2::aes(x = mean,
                                    y = reps)) +
    ggplot2::geom_point(ggplot2::aes(color = colorCI), size = 6, shape = 18) +                       # Adding confidence intervals to ggplot2 plot
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower,
                                         xmax = upper, 
                                         color = colorCI)) +
    ggplot2::scale_color_manual(values = c("A" = "black", "B" = "red")) +
    ggplot2::geom_vline(data = data.frame(options$mu),
                        ggplot2::aes(xintercept = options$mu),
                        linetype = "dashed")
  
  p <- p +
    jaspGraphs::geom_rangeframe() + # add lines on the x-axis and y-axis
    jaspGraphs::themeJaspRaw()      # add the JASP theme
  
  singlePlot <- createJaspPlot(title = "", width = 480, height = 320)
  treePlotContainer[["treePlot"]] <- singlePlot  
  
  if(isTryError(p))
    singlePlot$setError(.extractErrorMessage(p))
  else
    singlePlot$plotObject <- p
  
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
