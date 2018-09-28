##' Conversion by General Equivalence Mappings (GEMs)
##'
##' This function converts the diagnosis codes to a different version by the
##' General Equivalence Mappings (GEMs) developed by the National Center for
##' Health Statistics, Centers for Medicare and Medicaid Services (CMS),
##' AHIMA, the American Hospital Association, and 3M Health Information
##' Systems.
##'
##' The CMS GEMs consist of the forward mapping (from ICD-9 codes to ICD-10
##' codes) and the backward mapping (from ICD-10 codes to ICD-9 codes).  In
##' addition to these two directions, the Agency for Healthcare Research and
##' Quality (AHRQ) also proposed the reverse mappings.
##'
##' @param dx A character vector or list of character vectors representing
##'     diagnosis codes of a number of patients.  The length of this argument
##'     should be the number of the patients.
##' @param direction A character string specifying the conversion direction.
##'     The possible options are \code{"forward"}, \code{"reverse-forward"},
##'     \code{"backward"}, \code{"reverse-backward"}.
##' @param year A numeric value or character string specifying the year of the
##'     CMS GEMs for conversion.
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


## TODO: the function that does the AHRQ mapping
ahrq_map <- function(keys, direction = c("9=>10", "10=>9"),
                     decimal = FALSE, simplify = FALSE, ...)
{
    ## internal function that concatenate unique values
    cat_unique <- function(dx1, dx2) {
        if (length(dx1) != length(dx2))
            stop("The numbers of the diagnosis codes are different.")
        lapply(seq_along(dx1), function(i) {
            sort(unique(c(dx1[[i]], dx2[[i]])))
        })
    }

    ## get direction
    direction <- match.arg(direction)

    ## from ICD-9 to ICD-10
    if (direction == "9=>10") {
        ## stage 1. map icd-9 to icd-10
        ## stage 1.1. by forward mapping
        stage11_values <- icd_map(keys, direction = "forward")
        ## stage 1.2. by reverse-backward map
        stage12_values <- icd_map(keys, direction = "reverse-backward")
        ## combine (unique) values from stage 1.
        stage1_values <- cat_unique(stage11_values, stage12_values)

        ## stage 2. map icd-10 to icd-9
        ## stage 2.1. by backward mapping
        stage21_values <- icd_map(stage1_values, direction = "backward")
        ## stage 2.2. by reverse-backward mapping
        stage22_values <- icd_map(stage1_values, direction = "reverse-backward")
        ## combine (unique) values from stage 2.
        stage2_values <- cat_unique(stage21_values, stage22_values)

        ## stage 3. map icd-9 to icd-10 (again)
        ## stage 3.1. by forward mapping
        stage31_values <- icd_map(stage2_values, direction = "forward")
        ## stage 3.2. by reverse-backward map
        stage32_values <- icd_map(stage2_values, direction = "reverse-backward")
        ## combine (unique) values from stage 3.
        resList <- cat_unique(stage31_values, stage32_values)
    } else {
        ## otherwise, from ICD-10 to ICD-9
        ## stage 1. map icd-10 to icd-9
        ## stage 1.1. by backward mapping
        stage11_values <- icd_map(keys, direction = "backward")
        ## stage 1.2. by reverse-forward map
        stage12_values <- icd_map(keys, direction = "reverse-forward")
        ## combine (unique) values from stage 1.
        resList <- cat_unique(stage11_values, stage12_values)
    }

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
