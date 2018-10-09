##' Translatation of ICD Codes by General Equivalence Mappings (GEMs)
##'
##' This function provides an open-source implementation in R in addition to
##' the Mapping tool developed by the Agency for Healthcare Research and
##' Quality (AHRQ).  It translates the ICD diagnosis codes to the a different
##' version by the General Equivalence Mappings (GEMs) developed by the
##' National Center for Health Statistics, Centers for Medicare and Medicaid
##' Services (CMS), AHIMA, the American Hospital Association, and 3M Health
##' Information Systems.  The CMS GEMs currently consist of the forward
##' mapping from ICD-9 codes to ICD-10 codes and the backward mapping from
##' ICD-10 codes to ICD-9 codes.  In addition to these two mappings, the
##' Agency for Healthcare Research and Quality (AHRQ) also proposed
##' translation by using the reverse mappings.
##'
##' Taking the translation from ICD-9 codes to ICD-10 codes as an example, the
##' procedure is elaborated as follows: In stage one, the input ICD-9 codes
##' are mapped to ICD-10 codes using the ICD-9 to ICD-10 forward map as well
##' as the reverse of the ICD-10 to ICD-9 backward map.  If \code{multiStage =
##' FALSE}, the procedure will return the translation results from stage one
##' (and skip the following stages).  Otherwise, the procedure will continue
##' and become a multiple stage process.  In stage two, the ICD-10 codes
##' output from the stage one are mapped back to ICD-9 codes using the
##' backward map as well as the reverse of the forward map; In stage three, it
##' applies the forward map and reverse-backward map used in stage one again
##' to the ICD-9 codes from the stage two and return the resulting ICD-10
##' codes.
##'
##' @name icd_map
##'
##' @usage
##' icd_map(dx, from = 9, to = 10, year = 2018,
##'         method = c("gem", "reverse-gem", "both", "multi-stage"),
##'         decimal = FALSE, nomatch = c('""', NA),
##'         output = c("character", "list", "tidy-data"), cache = TRUE, ...)
##'
##' @param dx A character vector representing diagnosis codes.  Each element
##'     of the vector can either represent individual diagnosis code or a set
##'     of diagnosis codes that are concartenated by commas in between.
##' @param from A integer value specifying the original code version.
##'     Currently, the available options are \code{9} or \code{10}.
##' @param to A integer value specifying the original code version.
##'     Currently, the available options are \code{9} or \code{10}.  If the
##'     input \code{from} and \code{to} are the same, the function will skip
##'     all the translation and return the input \code{dx} with a warning.
##' @param method A character string specifying the translateion method.  The
##'     available options are \code{"gem"} for CMS GEM, \code{"reverse-gem"}
##'     for the reverse of CMS GEM, \code{"both"} for both GEM and reverse
##'     GEM, \code{"multi-stage"} for multiple stage procedure.
##' @param year A numeric value specifying the year of the CMS GEMs.  The
##'     currently available options are \code{2017} and \code{2018}.  By
##'     default, 2018 CMS GEMs is used.
##' @param decimal A logical value.  If \code{TRUE}, the diagnosis codes would
##'     be returned with decimal points.  The default is \code{FALSE}.
##' @param nomatch A character string indicating no translation result can be
##'     found through the specified mapping.  By default, empty strings,
##'     \code{""}, will be used.  Another available option is \code{NA} (or
##'     more specific \code{NA_character_}).  In that case, the code will be
##'     translated to \code{NA_character_} if no translataion result can be
##'     found.
##' @param output A character value specifying the format of the output.  The
##'     avaiable options are \code{"character"}, \code{"list"}, and
##'     \code{"tidy-data"}.  By default, option \code{"character"} is used and
##'     results in a character vector that consists of element-wise
##'     concatenatation by commas of all the translated diagnosis codes from
##'     the original codes.  If \code{"list"} is specified, all the translated
##'     codes will not be concartenated and a list of character vectors will
##'     be returned by the function.  Similarly, if \code{"tidy-data"} is
##'     specified, a data frame in a tidy format will be returned.  The first
##'     column of the data frame consists of the original diagnosis codes; the
##'     second column consists of the translated diagnosis codes.
##' @param cache A logical value specifying whether to cache all the mappings
##'     for \code{method = "both"} (both CMS GEM and its reverse mapping), and
##'     \code{method = "multi-stage"} (the multiple stage procedure).  If
##'     \code{TRUE} by default, the specified mapping will be generated,
##'     cached and, applied to the translation.  If \code{FALSE}, the CMS GEM
##'     and its reverse mapping will be used for translatation every time
##'     without cache.  It is recommended to set \code{cache = TRUE} for
##'     translation from ICD-9 to ICD-10.  For translation from ICD-10 to
##'     ICD-9, the caching process only takes noticeable time (usually several
##'     minutes at most) for the multi-stage procedure.
##' @param ... Other arguments for future usage.  A warning will be thrown out
##'     if any argument goes into \code{...} accidentally.
##'
##' @author Wenjie Wang <wenjie.2.wang@uconn.edu>
##'
##' @return A character vector of the same length with the input vector will
##'     be returned by default or if \code{output = "charactor"}.  A list of
##'     character vectors will be returned if \code{output = "list"}; A data
##'     frame in tidy-format will be returned if \code{output = "tidy-data"}.
##'     See argument \code{output} for details.
##'
##' @references
##'
##' 2017-ICD-10-CM-and-GEMs.  The U.S. Centers for Medicare & Medicaid
##' Services. 22 August, 2016.
##' \url{https://www.cms.gov/Medicare/Coding/ICD10/2017-ICD-10-CM-and-GEMs.html}.
##' Accessed 28 September, 2018.
##'
##'
##' 2018-ICD-10-CM-and-GEMs.  The U.S. Centers for Medicare & Medicaid
##' Services. 11 August, 2017.
##' \url{https://www.cms.gov/Medicare/Coding/ICD10/2018-ICD-10-CM-and-GEMs.html}.
##' Accessed 28 September, 2018.
##'
##'
##' The AHRQ MapIT Automated In-house Stand-alone Mapping Tool. Agency for
##' Healthcare Research and Quality. 26 March, 2018.
##' \url{https://www.qualityindicators.ahrq.gov/resources/toolkits.aspx}.
##' Accessed 28 September, 2018.
##'
##' @seealso \code{\link{find_billable}}
##'
##' @examples
##' library(touch)
##'
##' ### some random ICD-9 and ICD-10 codes
##' icd9codes <- c("0011", "001.1", "316", "29383", "E9808", "V90")
##' icd10codes <- c("F0390", "F0630", "F54", "F30.13", "A010", "M61019")
##'
##' ### forward mapping from ICD-9 to ICD-10
##' icd_map(icd9codes)
##' icd_map(icd9codes, decimal = TRUE, nomatch = NA)
##'
##' ### backward mapping from ICD-10 to ICD-9
##' icd_map(icd10codes, from = 10, to = 9)
##' icd_map(icd10codes, from = 10, to = 9, nomatch = NA, output = "list")
##' icd_map(icd10codes, from = 10, to = 9,
##'         decimal = TRUE, nomatch = NA, output = "tidy")
##'
##' ### reverse-backward mapping from ICD-9 to ICD-10
##' icd_map(icd9codes, method = "reverse-gem")
##' icd_map(icd9codes, method = "reverse", decimal = TRUE, nomatch = NA)
##'
##' ### reverse-forward mapping from ICD-10 to ICD-9
##' icd_map(icd10codes, from = 10, to = 9, method = "reverse-gem")
##' icd_map(icd10codes, from = 10, to = 9, method = "reverse",
##'         decimal = TRUE, nomatch = NA)
##'
##' ### forward and reverse-backward mapping from ICD-9 to ICD-10
##' icd_map(icd9codes, method = "both")
##' icd_map(icd9codes, method = "both", decimal = TRUE, nomatch = NA)
##'
##' ### backward and reverse-forward mapping from ICD-10 to ICD-9
##' icd_map(icd10codes, from = 10, to = 9, method = "both")
##' icd_map(icd10codes, from = 10, to = 9, method = "both",
##'         decimal = TRUE, nomatch = NA)
##'
##' ### multi-stage process mapping ICD-9 to ICD-10
##' icd_map(icd9codes, method = "multi-stage")
##' icd_map(icd9codes, method = "multi-stage", decimal = TRUE, nomatch = NA)
##'
##' ### multi-stage process mapping ICD-10 to ICD-9
##' icd_map(icd10codes, from = 10, to = 9,
##'         method = "multi-stage", cache = FALSE)
##' icd_map(icd10codes, from = 10, to = 9, method = "multi-stage",
##'         decimal = TRUE, nomatch = NA, cache = FALSE)
##' @export
icd_map <- function(dx, from = 9, to = 10, year = 2018,
                    method = c("gem", "reverse-gem", "both", "multi-stage"),
                    decimal = FALSE, nomatch = c('""', NA),
                    output = c("character", "list", "tidy-data"),
                    cache = TRUE, ...)
{
    ## check from and to
    if (! from %in% c(9, 10))
        stop(sprintf("The translation from version %s is not supported.",
                     as.character(from)))
    if (! to %in% c(9, 10))
        stop(sprintf("The translation to version %s is not supported.",
                     as.character(to)))
    ## check year
    if (! year %in% c(2017, 2018))
        stop(sprintf("The year %s is not supported.",
                     as.character(year)))

    ## throw warnings if `...` is used by mistake
    dotList <- list(...)
    if (length(dotList) > 0)
        warning("Some arguments went into `...`, ",
                "which is however not used currently.")

    ## early stop if from == to
    if (from == to) {
        warning("The original dx code was returned ",
                "because the specified `from` and `to` are the same.")
        return(dx)
    }

    ## match method
    method <- match.arg(method)
    ## match the string for nomatch
    nomatch <- match.arg(as.character(nomatch), c('""', NA_character_))
    ## match output
    output <- match.arg(output)

    ## switch table for map: year number + from + to + method

    ## 1709101: 2017, 9 to 10, forward
    ## 1709102: 2017, 9 to 10, reverse-backward
    ## 1709103: 2017, 9 to 10, forward + reverse-backward
    ## 1709104: 2017, 9 to 10, 3 stages

    ## 1710091: 2017, 10 to 9, backward
    ## 1710092: 2017, 10 to 9, reverse-forward
    ## 1710093: 2017, 10 to 9, backward + reverse-forward
    ## 1710094: 2017, 10 to 9, 3 stages

    ## 1809101: 2018, 9 to 10, forward
    ## 1809102: 2018, 9 to 10, reverse-backward
    ## 1809103: 2018, 9 to 10, forward + reverse-backward
    ## 1809104: 2018, 9 to 10, 3 stages

    ## 1810091: 2018, 10 to 9, backward
    ## 1810092: 2018, 10 to 9, reverse-forward
    ## 1810093: 2018, 10 to 9, backward + reverse-forward
    ## 1810094: 2018, 10 to 9, 3 stages

    year_idx <- as.integer(year) - 2000L
    method_idx <- switch(method,
                         "gem" = 1L,
                         "reverse-gem" = 2L,
                         "both" = 3L,
                         "multi-stage" = 4L)
    map_id <- sprintf("%d%02d%02d%1d", year_idx, from, to, method_idx)
    map_id <- as.integer(map_id)

    ## re-format dx
    dx <- toupper(remove_dot(trimws(dx)))

    ## call the rcpp routine
    res <- rcpp_gem(dx, map_id, cache)

    ## replace empty strings with NA if nomath is NA
    if (is.na(nomatch))
        res <- empty2na(res)
    ## add decimal if needed
    if (decimal) {
        res <- rcpp_strsplit(res)
        if (to == 9) {
            ## mapped from icd-10 to icd-9
            res <- lapply(res, insert_dot, version = 9)
        } else if (to == 10) {
            ## mapped from icd-9 to icd-10
            res <- lapply(res, insert_dot, version = 10)
        } else {
            ## not actually useful now but it makes clear for future
            stop("Do not know how to insert dots. ",
                 "Code version is not supported.")
        }
        if (output == "character") {
            res <- rcpp_strcat(res)
        } else if (output == "tidy-data") {
            col_names <- paste0("ICD-", c(from, to))
            res <- dx_list2tidy(res, x_names = dx, col_names = col_names)
        }
    } else if (output != "character") {
        res <- rcpp_strsplit(res)
        if (output == "tidy-data") {
            col_names <- paste0("ICD-", c(from, to))
            res <- dx_list2tidy(res, x_names = dx, col_names = col_names)
        }
    }
    ## return
    res
}


##' Find Billable ICD Codes from CMS GEMs
##'
##' This function tries to find all the billable ICD codes that can be
##' translated by CMS GEMs for each of the input diagnosis codes representing
##' a major category.
##'
##' It is designed to be used with the function \code{\link{icd_map}} for
##' translating the diagnosis codes that are not billable but representing
##' major categories.  Notice that only the character vector output can be
##' directly passed to the function \code{\link{icd_map}} for translation.
##'
##' @name find_billable
##'
##' @usage
##' find_billable(dx, version = 10, year = 2018,
##'               aggressive_match = TRUE, decimal = FALSE,
##'               output = c("character", "list", "tidy-data"), ...)
##'
##' @param version A numeric value specifying the version of the diagnosis
##'     codes that should be either \code{9} for ICD-9 codes or \code{10} for
##'     ICD-10 codes.
##' @param aggressive_match A logical value specifying the aggressiveness for
##'     finding all billable codes based on the input diagnosis category.  If
##'     \code{TRUE} (the default), the function will add the regular
##'     expression \code{"[[[[:alnum:]]{1,4}]]"} to the tail of diagnosis
##'     category so that all the billable diagnosis codes under the given
##'     category will be matched.  If \code{FALSE}, the function will add the
##'     regular experssion \code{"[[:alnum:]]"} recursively at most four times
##'     until any set of billable codes are matched.
##'
##' @inheritParams icd_map
##'
##' @return A character vector of the same length with the input vector will
##'     be returned by default or if \code{output = "charactor"}.  A list of
##'     character vectors will be returned if \code{output = "list"}; A data
##'     frame in tidy-format will be returned if \code{output = "tidy-data"}.
##'     See argument \code{output} for details.
##'
##' @seealso icd_map
##'
##' @examples
##' library(touch)
##'
##' ### for ICD-9 codes
##' icd9_major <- c("001", "316", "808", NA, "not_a_dx")
##'
##' ## find all billable codes under the major category
##' find_billable(icd9_major, version = 9)
##'
##' ## find the billable codes right under the major category
##' (icd9_billable <- find_billable(icd9_major, version = 9,
##'                                 aggressive_match = FALSE))
##'
##' ## compare the translation results
##' icd_map(icd9_major, nomatch = NA)
##' icd_map(icd9_billable, nomatch = NA)
##'
##' ### for ICD-10 codes
##' icd10_major <- c("T36.0X2", "T36.3X2", "T38.6X2")
##'
##' ## find all billable codes and output in different formats
##' find_billable(icd10_major, version = 10)
##' find_billable(icd10_major, version = 10, output = "list")
##' find_billable(icd10_major, version = 10, output = "tidy-data")
##'
##' ## add decimal if wanted
##' (icd10_billable <- find_billable(icd10_major, version = 10, decimal = TRUE))
##'
##' ## compare the translation results
##' icd_map(icd10_major, from = 10, to = 9, nomatch = NA)
##' icd_map(icd10_billable, from = 10, to = 9)
##' @export
find_billable <- function(dx, version = 10, year = 2018,
                          aggressive_match = TRUE, decimal = FALSE,
                          output = c("character", "list", "tidy-data"),
                          ...)
{
    ## check year
    if (! year %in% c(2017, 2018))
        stop(sprintf("The year %s is not supported.",
                     as.character(year)))

    ## throw warnings if `...` is used by mistake
    dotList <- list(...)
    if (length(dotList) > 0)
        warning("Some arguments went into `...`, ",
                "which is however not used currently.")

    ## match output
    output <- match.arg(output)

    ## re-format dx
    dx <- toupper(remove_dot(trimws(dx)))

    ## internal engine function
    regex_billable <- function(x, pool, aggressive_match) {
        if (is.na(x)) return(x)
        if (x %in% pool) return(x)
        if (aggressive_match) {
            out <- grep(paste0("^", x, "[[:alnum:]]{1,4}$"),
                        pool, value = TRUE)
            if (length(out) > 0) return(out)
        } else {
            ## recursively match with additional trailing numbers or letters
            for (i in 1:4) {
                x <- paste0(x, "[[:alnum:]]")
                out <- grep(paste0("^", x, "$"), pool, value = TRUE)
                if (length(out) > 0) return(out)
            }
        }
        return(NA_character_)
    }

    ## main part
    if (version == 9) {
        map_f <- get(sprintf("forward_map_%d", year))
        map_rb <- get(sprintf("reverse_backward_map_%d", year))
        icd9_billable <- unique(c(map_f$icd9_codes, map_rb$icd9_codes))
        res <- lapply(dx, regex_billable, pool = icd9_billable,
                      aggressive_match = aggressive_match)
        ## add decimal if needed
        if (decimal)
            res <- lapply(res, insert_dot, version = 9)
    } else if (version == 10) {
        map_b <- get(sprintf("backward_map_%d", year))
        map_rf <- get(sprintf("reverse_forward_map_%d", year))
        icd10_billable <- unique(c(map_b$icd10_codes, map_rf$icd10_codes))
        res <- lapply(dx, regex_billable, pool = icd10_billable,
                      aggressive_match = aggressive_match)
        ## add decimal if needed
        if (decimal)
            res <- lapply(res, insert_dot, version = 10)
    } else {
        stop(sprintf("The specified `version = %s` is not supported.",
                     as.character(version)))
    }

    ## prepare for output
    if (output == "character") {
        res <- rcpp_strcat(res)
    } else if (output == "tidy-data") {
        col_names <- paste0(c("ICD-", "Billable_ICD-"), version)
        res <- dx_list2tidy(res, x_names = dx, col_names = col_names)
    }

    ## return
    res
}
