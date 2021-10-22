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

  # anovaContainer <- .getAnovaContainer(jaspResults)

  return()
}

.getAnovaContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["anovaContainer"]])) {
    anovaContainer <- jaspResults[["anovaContainer"]]
  } else {
    anovaContainer <- createJaspContainer()
    # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
    anovaContainer$dependOn(c("dependent", "modelTerms", "covariates", "sumOfSquares", "wlsWeights", "customContrasts"))
    jaspResults[["anovaContainer"]] <- anovaContainer
  }
  return(anovaContainer)
}
