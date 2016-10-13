##' Comorbidity measures from AHRQ HCUP
##'
##' This function maps a matrix of ICD9 codes to AHRQ comorbidity measures.
##'
##'
##' @param icd a character matrix of icd9 codes, with rows representing
##' patients.
##' @param drg a numeric vector of drg codes, with length the same as
##' \code{nrow(icd)}.
##' @param needClean logical, TRUE means cleaning is needed (string trimming,
##' zero supplementation)
##' @param needPrep logical, TRUE means preparation is needed (convert char to
##' numeric)
##' @return a matrix with the same number of rows as the input and with the
##' comorbidity measures in columns
##' @author Jun Yan
##' @references Elixhauser et. al. (1998)
##' @keywords manipulation
##' @examples
##'
##' data(dxDat)
##' drg <- dxDat$drg
##' icd <- dxDat[, -1L]
##' output <- cmbd(icd, drg=drg)
##' @importFrom stringr str_trim
##' @export cmbd
cmbd <- function(icd, drg=NULL, needClean=TRUE, needPrep=TRUE) {
  if (!is.matrix(icd)) icd <- as.matrix(icd)
  if (needClean) icd <- icd9Clean(icd)
  if (needPrep) icd <- icd9Prep(icd)
  n <- nrow(icd)
  icd <- icd[, -1L]
  output <- sapply(1:length(cmbdFuns),
                   function(i) {
                     score <- matrix(cmbdFuns[[i]](icd), nrow=n)
                     score <- rowSums(score, na.rm=TRUE)
                     as.integer(score > 0)
                   })
  colnames(output) <- names(cmbdFuns)
  output <- data.frame(output)
  icd.s <- output[, -(1:30)] ## record the 10 groups for HTNCX
  output <- output[, 1:30] ## keep the 30 comorbidities
  output$HTN <- (1 - (output$HTNCX > 0))*output$HTN
  output$DM <- (1 - (output$DMCX > 0))*output$DM
  output$TUMOR <- (1 - (output$METS > 0))*output$TUMOR
  
  if (!is.null(drg)) {## check drg flags
    stopifnot(nrow(icd) == length(drg))
    flag <- drgFlag(drg)
    colnames(flag) <- names(drgFuns)
    flag <- data.frame(flag)
    nf <- ncol(flag)
    flag.s <- flag[, (nf-1):nf]
    flag <- flag[, -((nf-1):nf)]
    attach(icd.s)
    flag.HTNCX <- drgFuns.2[[1]](flag.s$CARDRG, flag.s$RENALDRG)
    flag.RENLFAIL <- drgFuns.2[[2]](flag.s$CARDRG, flag.s$RENALDRG)
    detach(icd.s)
    flag$HTNCX <- (flag$HTNCX+flag.HTNCX) > 0
    flag$RENLFAIL <- (flag$RENLFAIL+flag.RENLFAIL) > 0
    output <- output * (!flag)
  }
  
  output$HTN_C <- output$HTN + output$HTNCX
  output$HTN <- NULL
  output$HTNCX <- NULL
  output
}

drgFlag <- function(drg) {
  if (!is.vector(drg)) drg <- as.vector(drg)
  n <- length(drg)
  output <- sapply(1:length(drgFuns),
                   function(i) {
                     flag <- drgFuns[[i]](drg)
                   })
  output
}

## new measure for infection during childbirth
chibirInfection <- function(icd, needClean=FALSE, needPrep=FALSE) {
  if (!is.matrix(icd)) icd <- as.matrix(icd)
  if (needClean) icd <- icd9Clean(icd)
  if (needPrep) icd <- icd9Prep(icd)
  output <- (floor(icd) == 647) | (floor(icd) == 670) |
    (icd >= 659.30 & icd <= 659.33) |
      (icd >= 658.40 & icd <= 658.43) |
        (icd >= 646.60 & icd <= 646.64)
  output <- rowSums(output, na.rm=TRUE)
  (output > 0) * 1
}

icd9Clean <- function(input) {
  output <- trimws(input)
  nc <- nchar(output)
  output <- ifelse(nc == 3, paste(output, "00", sep=""), output)
  output <- ifelse(nc == 4, paste(output, "0",  sep=""), output)
  output
}


## convert string to numeric, with leading chars fixed
icd9Prep <- function(input) {
  ## assuming no lower cases
  input <- sub("^E", "11", input)
  input <- sub("^V", "12", input)
  output <- matrix(as.numeric(input) / 100, nrow=nrow(input))
  dimnames(output) <- dimnames(input)  
  output
}



## Note: The %in% function returns a vector, which may cause trouble
##       with rowSums
