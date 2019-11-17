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
##' @export cmbd
cmbd <- function(icd, drg = NULL, needClean = TRUE, needPrep = TRUE) {
  if (! is.matrix(icd)) icd <- as.matrix(icd)
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
  htn.cx <- output[, -(1:30)] # store 10 temporary groups for HTNCX
  output <- output[,   1:30 ] # store the 30 comorbidities for output

  ## nonsense, just to suppress the note from R CMD check --as-cran
  DMCX <- METS <- NULL

  ## only keep more severe comorbidity
  output <- within(output, HTN   <- ifelse(HTNCX, 0, HTN))
  output <- within(output, DM    <- ifelse(DMCX,  0, DM))
  output <- within(output, TUMOR <- ifelse(METS,  0, TUMOR))

  if (!is.null(drg)) { # check drg flags
    if (!is.vector(drg)) drg <- as.vector(drg)
    stopifnot(nrow(icd) == length(drg))
    flag <- sapply(1:length(drgFuns),
                   function(i) {
                       flag <- drgFuns[[i]](drg)
                   })
    colnames(flag) <- names(drgFuns)
    flag <- data.frame(flag)
    nf <- ncol(flag)
    flag.s <- flag[, -(1:(nf - 2))] # store the flag for CARDDRG and RENALDRG
    flag <- flag[, 1:(nf - 2)] # store the flags for outputted comorbidities
    ## compute the flags for two special comorbidities
    flag.2 <- sapply(1:length(specdrgFuns),
                   function(i) {
                     flag <- with(flag.s,
                                  specdrgFuns[[i]](htn.cx, CARDDRG, RENALDRG))
                   })
    flag <- within(flag, HTNCX <- (HTNCX + flag.2[, 1]) > 0)
    flag <- within(flag, RENLFAIL <- (RENLFAIL + flag.2[, 2]) > 0)
    output <- output * (!flag)
  }
  ## combine HTN and HTNCX to generate variable HTN_C
  output <- within(output, HTN_C <- HTN + HTNCX)
  output <- subset(output, select = -c(HTN, HTNCX))
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


##' Reformat Comorbidity Measures
##'
##' This function processes the character matrix of ICD9 codes by converting
##' them to character codes of length 5.  For SAS procedure from HCUP, it trims
##' all character string to be of length 5, adds the missing trailing
##' white space, and capitalizes the first character in ICD9 codes.
##'
##' @param input a character matrix of ICD9 codes, with rows representing
##' patients.
##' @param style a character vector of length one indicating the reformatting
##' style to follow.  The possible option are "touch" and "hcup". The former
##' does the cleaning for this package; The latter does the reformatting for
##' the SAS script provided by HCUP.
##'
##' @return a matrix of cleaned ICD9 codes.
##' @author Jun Yan and Wenjie Wang
##' @examples
##' data(dxDat)
##' icd <- dxDat[, -1L]
##' output <- icd9Clean(icd)
##' @export icd9Clean
icd9Clean <- function(input, style = c("touch", "hcup")) {
    style <- match.arg(style)
    if (! is.matrix(input)) input <- as.matrix(input)
    if (identical(style, "touch")) {
        output <- toupper(trimws(input))
        nc <- nchar(output)
        output <- ifelse(nc == 3, paste0(output, "00"), output)
        output <- ifelse(nc == 4, paste0(output, "0"), output)
        return(output)
    }
    ## else for sas script from hcup
    ## first trim at left side
    dx <- trimws(as.character(input), "left")
    ndx <- nchar(dx)
    ## cut at 5 for dx nchar greater than 5
    nCharGt5 <- dx[idxGt5 <- (ndx > 5)]
    dx[idxGt5] <- substring(nCharGt5, 1, 5)
    ## add trailling space for dx nchar less than 5
    nCharLt5 <- dx[idxLt5 <- (ndx < 5)]
    space5 <- paste(rep(" ", 5), collapse = "")
    dx[idxLt5] <- substring(paste0(nCharLt5, space5), 1, 5)
    ## capitalize
    dx <- toupper(dx)
    ## recover the dimensions
    dim(dx) <- dim(input)
    dx
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
