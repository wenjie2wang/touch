##
## R package touch by Wenjie Wang, Yan Li, and Jun Yan
## Copyright (C) 2015-2019
##
## This file is part of the R package touch.
##
## The R package touch is free software: You can redistribute it and/or
## modify it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or any later
## version (at your option). See the GNU General Public License at
## <https://www.gnu.org/licenses/> for details.
##
## The R package touch is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
##

##' Sample Data of Diagnosis Code
##'
##' A data frame of diagnosis code with variates named
##' \code{drg} and \code{dx1}, \code{dx2}, ..., \code{dx10},
##' where
##' \itemize{
##'     \item \code{drg}: drg code for comorbidity;
##'     \item \code{dx1-dx10}: icd-9 code for 10 diagnosises.
##' }
##'
##' @docType data
##' @name dxDat
##' @usage data(dxDat)
##' @format  A data frame with 1000 rows and 11 variables.
##' @keywords datasets
##' @examples
##' data(dxDat)
##' drg <- dxDat$drg
##' icd <- dxDat[, - 1L]
"dxDat"
