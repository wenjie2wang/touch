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

##' Insert Dot to ICD-9 and ICD-10 Diagnosis Codes
##'
##' This function adds dot to diagnosis codes of the given ICD version.
##'
##' @param dx A character vector for the diagnosis codes
##' @param version The version of the diagnosis codes.  Two available options
##'     are \code{10} and \code{9}.  The default version is \code{10} for
##'     ICD-10.  This argument can be either a numerical value (nummerical
##'     vector of length one) or a character string (character vector of
##'     length one).
##'
##' @return A character vector representing the diagnosis codes in decimal
##'     format.
##'
##' @examples
##' library(touch)
##'
##' ## for ICD-9 codes
##' icd9codes <- c("0011", "001.1", "316", "E950", "E9808", "V90", "v100")
##' insert_dot(icd9codes, 9)
##'
##' ## for ICD-10 codes
##' icd10codes <- c("A010", "M61019", "p52", "p528")
##' insert_dot(icd10codes)
##' @export
insert_dot <- function(dx, version = c(10, 9)) {
    version <- match.arg(as.character(version), c("10", "9"))
    dx <- toupper(remove_dot(trimws(dx)))
    if (version == "9") {
        ind <- as.integer(grepl("^E", dx))
        out <- substr(dx, 1L, 3L + ind)
        subcat <- substring(dx, 4L + ind)
    } else {
        out <- substr(dx, 1L, 3L)
        subcat <- substring(dx, 4L)
    }
    empty_idx <- subcat != ""
    out[empty_idx] <- paste0(out[empty_idx], ".", subcat[empty_idx])
    out
}


### internal function
## this funciton simply removes the dot from the dx codes
remove_dot <- function(dx) {
    gsub(".", "", dx, fixed = TRUE)
}

