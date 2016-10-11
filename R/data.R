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
