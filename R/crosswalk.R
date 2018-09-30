##' Translatation of ICD Codes by General Equivalence Mappings (GEMs)
##'
##' This function translates the ICD diagnosis codes to the a different
##' version by the General Equivalence Mappings (GEMs) developed by the
##' National Center for Health Statistics, Centers for Medicare and Medicaid
##' Services (CMS), AHIMA, the American Hospital Association, and 3M Health
##' Information Systems.
##'
##' The CMS GEMs consist of the forward mapping (from ICD-9 codes to ICD-10
##' codes) and the backward mapping (from ICD-10 codes to ICD-9 codes).  In
##' addition to these two directions, the Agency for Healthcare Research and
##' Quality (AHRQ) also proposed the reverse mappings.
##'
##' @name icd_map
##' @usage icd_map(dx, direction = c("forward", "reverse-forward",
##'                                  "backward", "reverse-backward"),
##'                year = c(2018, 2017), decimal = FALSE, simplify = FALSE,
##'                ...)
##'
##' @param dx A character vector or list of character vectors representing
##'     diagnosis codes of a number of patients.  The length of this argument
##'     should be the number of the patients.
##' @param direction A character string specifying the conversion direction.
##'     The possible options are \code{"forward"}, \code{"reverse-forward"},
##'     \code{"backward"}, \code{"reverse-backward"}.
##' @param year A numeric value or character string specifying the year of the
##'     CMS GEMs for conversion.  The available options are \code{2017} and
##'     \code{2018}.
##' @param decimal A logical value.  If \code{TRUE}, the diagnosis codes would
##'     be returned with decimal points.  The default is \code{FALSE}.
##' @param simplify A logical value specifying whether to simplify the output
##'     to a character vector that concatenates diagnosis codes.
##' @param ... Other arguments for future usage.
##'
##' @author Wenjie Wang <wenjie.2.wang@uconn.edu>
##'
##' @return A list of character vectors with the same length of the input
##'     vector or list.  If \code{simplify} is \code{TRUE}, a character vector
##'     will be returned instead, which aggregates each element of the list
##'     into a character string by concatenating with commas.
##'
##' @references
##'
##' 2017-ICD-10-CM-and-GEMs.  The U.S. Centers for Medicare & Medicaid
##' Services. 22 August, 2016.
##' \url{https://www.cms.gov/Medicare/Coding/ICD10/2017-ICD-10-CM-and-GEMs.html}.
##' Accessed 28 September, 2018.
##'
##' 2018-ICD-10-CM-and-GEMs.  The U.S. Centers for Medicare & Medicaid
##' Services. 11 August, 2017.
##' \url{https://www.cms.gov/Medicare/Coding/ICD10/2018-ICD-10-CM-and-GEMs.html}.
##' Accessed 28 September, 2018.
##'
##' The AHRQ MapIT Automated In-house Stand-alone Mapping Tool. Agency for
##' Healthcare Research and Quality. 26 March, 2018.
##' \url{https://www.qualityindicators.ahrq.gov/resources/toolkits.aspx}.
##' Accessed 28 September, 2018.
##'
##' @seealso \code{\link{ahrq_map}} for translation by AHRQ mappings.
##' @examples
##' library(touch)
##'
##' ## some random ICD-9 and ICD-10 codes
##' icd9codes <- c("0011", "001.1", "316", "29383", "E9808", "V90")
##' icd10codes <- c("F0390", "F0630", "F54", "F30.13", "A010", "M61019")
##'
##' ## forward mapping from ICD-9 to ICD-10
##' icd_map(icd9codes)
##' icd_map(icd9codes, decimal = TRUE, simplify = TRUE)
##'
##' ## backward mapping from ICD-10 to ICD-9
##' icd_map(icd10codes, "backward")
##' icd_map(icd10codes, "backward", decimal = TRUE, simplify = TRUE)
##'
##' ## reverse-backward mapping from ICD-9 to ICD-10
##' icd_map(icd9codes, "reverse-backward")
##' icd_map(icd9codes, "reverse-backward", decimal = TRUE, simplify = TRUE)
##'
##' ## reverse-forward mapping from ICD-10 to ICD-9
##' icd_map(icd10codes, "reverse-forward")
##' icd_map(icd10codes, "reverse-forward", decimal = TRUE, simplify = TRUE)
##' @export
icd_map <- function(dx,
                    direction = c("forward", "reverse-forward",
                                  "backward", "reverse-backward"),
                    year = c(2018, 2017),
                    decimal = FALSE,
                    simplify = FALSE,
                    ...)
{
    ## match direction
    direction <- match.arg(direction)
    ## match year
    year <- match.arg(as.character(year), c("2018", "2017"))

    ## for vector input
    if (! is.list(dx)) {
        ## covnert dx to character strings if needed
        if (! is.character(dx))
            dx <- as.character(dx)
        dx <- strsplit(dx, split = ",")
    }

    ## call icd_map_more
    resList <- icd_map_more(dx, direction, year)

    ## add decimal if needed
    if (decimal) {
        if (direction %in% c("reverse-forward", "backward")) {
            ## mapped from icd-10 to icd-9
            resList <- lapply(resList, insert_dot, version = 9)
        } else {
            ## mapped from icd-9 to icd-10
            resList <- lapply(resList, insert_dot, version = 10)
        }
    }
    ## return vector if simplify = TRUE
    if (simplify) {
        return(sapply(resList, paste, collapse = ","))
    }
    ## return
    resList
}


##' Translatation of ICD Codes by AHRQ Mappings
##'
##' This function is an open-source implementation in R of the Mapping tool
##' developed by the Agency for Healthcare Research and Quality (AHRQ).  It
##' converts the ICD diagnosis codes to a different version the General
##' Equivalence Mappings (GEMs) and their reverse mappings.
##'
##' The procedure will be a multiple stage process if \code{nstage = 3} is
##' specified.  Taking the translation from ICD-9 codes to ICD-10 codes as an
##' example, the procedure is elaborated as follows: In stage one, the input
##' ICD-9 codes are mapped to ICD-10 codes using the ICD-9 to ICD-10 forward
##' map as well as the reverse of the ICD-10 to ICD-9 backward map; In stage
##' two, the ICD-10 codes output from the stage one are mapped back to ICD-9
##' codes using the backward map as well as the reverse of the forward map;
##' In stage three, we apply the forward map and reverse-backward map used in
##' stage one again to the ICD-9 codes from stage two and get the resulting
##' ICD-10 codes.
##'
##' @name ahrq_map
##' @usage ahrq_map(dx, direction = c("9=>10", "10=>9"),
##'                 nstage = c(1, 3), year = c(2018, 2017),
##'                 decimal = FALSE, simplify = FALSE, ...)
##'
##' @param dx A character vector or list of character vectors representing
##'     diagnosis codes of a number of patients.  The length of this argument
##'     should be the number of the patients.
##' @param direction A character string specifying the conversion direction.
##'     The possible options are \code{"9=>10"} and \code{"10=>9"}.
##' @param nstage A numeric value or character string specifying the number of
##'     stages that the mapping procedure has.  The possible options are
##'     \code{1} and \code{3}.  A one-stage process would be used by default.
##' @param year A numeric value or character string specifying the year of the
##'     CMS GEMs for conversion.  The currently available options are
##'     \code{2017} and \code{2018}.  The CMS GEMs of year 2018 is used by
##'     default.
##' @param decimal A logical value.  If \code{TRUE}, the diagnosis codes would
##'     be returned with decimal points.  The default is \code{FALSE}.
##' @param simplify A logical value specifying whether to simplify the output
##'     to a character vector that concatenates diagnosis codes.
##' @param ... Other arguments for future usage.
##'
##' @author Wenjie Wang <wenjie.2.wang@uconn.edu>
##'
##' @return A list of character vectors with the same length of the input
##'     vector or list.  If \code{simplify} is \code{TRUE}, a character vector
##'     will be returned instead, which aggregates each element of the list
##'     into a character string by concatenating with commas.
##'
##' @references
##'
##' The AHRQ MapIT Automated In-house Stand-alone Mapping Tool. Agency for
##' Healthcare Research and Quality. 26 March, 2018.
##' \url{https://www.qualityindicators.ahrq.gov/resources/toolkits.aspx}.
##' Accessed 28 September, 2018.
##'
##' @seealso \code{\link{icd_map}} for translation by GEMs.
##' @examples
##' library(touch)
##'
##' ## some random ICD-9 and ICD-10 codes
##' icd9codes <- c("0011", "001.1", "316", "29383", "E9808", "V90")
##' icd10codes <- c("F0390", "F0630", "F54", "F30.13", "A010", "M61019")
##'
##' ## forward mapping from ICD-9 to ICD-10
##' ahrq_map(icd9codes)
##' ahrq_map(icd9codes, decimal = TRUE, simplify = TRUE)
##'
##' ## backward mapping from ICD-10 to ICD-9
##' ahrq_map(icd10codes, "10=>9")
##' ahrq_map(icd10codes, "10", decimal = TRUE, simplify = TRUE)
##' @export
ahrq_map <- function(dx, direction = c("9=>10", "10=>9"),
                     nstage = c(1, 3), year = c(2018, 2017),
                     decimal = FALSE, simplify = FALSE, ...)
{
    ## get direction
    direction <- match.arg(direction)
    ## get number of stages
    nstage <- match.arg(as.character(nstage), c("1", "3"))

    if (direction == "9=>10") {
        ## from ICD-9 to ICD-10
        resList <- ahrq_map_9to10(dx, year)
        if (nstage == "3") {
            tmp <- ahrq_map_10to9(resList, year)
            resList <- ahrq_map_9to10(tmp, year)
        }
    } else {
        ## otherwise, from ICD-10 to ICD-9
        resList <- ahrq_map_10to9(dx, year)
        if (nstage == "3") {
            tmp <- ahrq_map_9to10(resList, year)
            resList <- ahrq_map_10to9(tmp, year)
        }
    }

    ## add decimal if needed
    if (decimal) {
        if (direction == "10=>9") {
            ## mapped from icd-10 to icd-9
            resList <- lapply(resList, insert_dot, version = 9)
        } else {
            ## mapped from icd-9 to icd-10
            resList <- lapply(resList, insert_dot, version = 10)
        }
    }
    ## return vector if simplify = TRUE
    if (simplify) {
        return(sapply(resList, paste, collapse = ","))
    }

    ## return
    resList
}


### internal function ==========================================================
## engine function (as simple and fast as possible)
icd_map_one <- function(uni_keys, direction, year)
{
    ## get the map of the given year
    .forward_map <- get(sprintf("forward_map_%s", year))
    .reverse_forward_map <- get(sprintf("reverse_forward_map_%s", year))
    .backward_map <- get(sprintf("backward_map_%s", year))
    .reverse_backward_map <- get(sprintf("reverse_backward_map_%s", year))
    ## map for one direction
    switch(direction,
           "forward" = {
               my_keys <- paste0("icd9_", remove_dot(uni_keys))
               resList <- mget(my_keys, envir = .forward_map,
                               ifnotfound = list(NA_character_))
           },
           "reverse-forward" = {
               my_keys <- paste0("icd10_", remove_dot(uni_keys))
               resList <- mget(my_keys, envir = .reverse_forward_map,
                               ifnotfound = list(NA_character_))
           },
           "backward" = {
               my_keys <- paste0("icd10_", remove_dot(uni_keys))
               resList <- mget(my_keys, envir = .backward_map,
                               ifnotfound = list(NA_character_))
           },
           "reverse-backward" = {
               my_keys <- paste0("icd9_", remove_dot(uni_keys))
               resList <- mget(my_keys, envir = .reverse_backward_map,
                               ifnotfound = list(NA_character_))
           })
    ## note that `sort` removes NA's
    sort(unique(do.call(c, lapply(resList, strsplit2vec))),
         na.last = NA)
}

## a simple wrapper for icd_map_one that takes concatenated dx codes
icd_map_more <- function(keys, direction, year)
{
    ## call icd_map_one
    lapply(seq_along(keys), function(i) {
        icd_map_one(keys[[i]], direction, year)
    })
}

## one stage mapping
ahrq_map_9to10 <- function(resList, year) {
    ## 1. by forward mapping
    res_1 <- icd_map(resList, direction = "forward", year = year)
    ## 2. by reverse-backward mapping
    res_2 <- icd_map(resList, direction = "reverse-backward", year = year)
    ## combine (unique) values
    cat_unique(res_1, res_2)
}
ahrq_map_10to9 <- function(resList, year) {
    ## 1. by backward mapping
    res_1 <- icd_map(resList, direction = "backward", year = year)
    ## 2. by reverse-forward mapping
    res_2 <- icd_map(resList, direction = "reverse-forward", year = year)
    ## combine (unique) values
    cat_unique(res_1, res_2)
}

## a simple function that concatenates unique values
cat_unique <- function(dx1, dx2) {
    if (length(dx1) != length(dx2))
        stop("The numbers of the diagnosis codes are different.")
    lapply(seq_along(dx1), function(i) {
        sort(unique(c(dx1[[i]], dx2[[i]])))
    })
}

