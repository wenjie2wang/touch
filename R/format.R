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
        ind <- as.integer(grepl("^[Ee]", dx))
        out <- substr(dx, 1L, 3L + ind)
        minors <- substring(dx, 4L + ind)
    } else {
        out <- substr(dx, 1L, 3L)
        minors <- substring(dx, 4L)
    }
    empty_idx <- minors != ""
    out[empty_idx] <- paste0(out[empty_idx], ".", minors[empty_idx])
    out
}


### internal function
## this funciton simply removes the dot from the dx codes
remove_dot <- function(dx) {
    gsub(".", "", dx, fixed = TRUE)
}

