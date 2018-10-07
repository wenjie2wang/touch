## aggregate diagnosis codes
aggregate_dx <- function(dx_codes, ...)
{
    dx <- c(dx_codes, ...)
    ## remove empty strings and NA's
    idx <- is.na(dx) | dx == ""
    dx <- dx[! idx]
    u_dx <- sort(unique(strsplit2vec(dx)))
    if (length(u_dx) > 1) {
        paste(u_dx, collapse = ",")
    } else if (length(u_dx) == 0L) {
        ""
    } else
        u_dx
}

## strsplit to vector
strsplit2vec <- function(x, split = ",", ...) {
    do.call(c, strsplit(x, split = split, ...))
}

## a simple function to combine multiple RData files into one RData file
combine_rda <- function(in_file, ..., out_file)
{
    rda_list <- list(in_file, ...)
    rda_env <- new.env()
    for (i in seq_along(rda_list)) {
        load(rda_list[[i]], rda_env)
    }
    save(list = ls(rda_env), file = out_file, envir = rda_env,
         compress = TRUE, compression_level = 9)
}


##' Check, Install and Attach Multiple R packages Specified
##'
##' The function is a simple wrapper that aims to simplify the process of
##' attaching multiple packages that are not necessarily available.  It first
##' checks whether the packages given were already installed and would try to
##' install them from specified repository if needed.  At last, it attachs all
##' the needed packages to the search path.
##'
##' See the source code for details.
##'
##' @param pkgs A character vector specifying the packages needed to reproduce
##'     this document.
##' @param repos A character vector for the base URL(s) of the repositories
##'     containing the source.  The default mirror is
##'     \code{"https://cloud.r-project.org"} and will be used if \code{repos}
##'     is not set.
##' @param ... Other arguments passed to function
##'     \code{\link[utils]{install.packages}}.
##'
##' @return \code{NULL} invisibly.
##' @examples
##' \dontrun{
##' need.pacakges(c("splines2", "reda"))
##' }
##' @importFrom utils installed.packages install.packages
##' @export
need.packages <- function(pkgs, repos = getOption("repos"), ...)
{
    new.pkgs <- pkgs[! (pkgs %in% installed.packages()[, "Package"])]
    if (length(new.pkgs) > 0) {
        if (is.null(repos) || repos == "@CRAN@") {
            repos <- "https://cloud.r-project.org"
        }
        install.packages(pkgs = new.pkgs, repos = repos, ...)
    }
    sapply(pkgs, function(a) {
        suppressMessages(require(a, character.only = TRUE))
    })
    invisible()
}
