## aggregate diagnosis codes
aggregate_dx <- function(dx, flags = NULL)
{
    ## remove empty strings and NA's
    idx <- is.na(dx) | dx == ""
    dx <- dx[! idx]
    if (length(dx) > 1) {
        if (is.null(flags)) {
            return(paste(dx, collapse = ","))
        }
        flags <- flags[! idx]
        ## combination flags
        flag3 <- as.integer(substr(flags, start = 3, stop = 3))
        is_comb <- ! is.na(flag3) & flag3 == 1L
        if (any(is_comb)) {
            dx1 <- dx[! is_comb]
            comb_dx <- dx[is_comb]
            comb_flags <- flags[is_comb]
            ## scenario flags
            flag4 <- as.integer(substr(comb_flags, start = 4, stop = 4))
            ## choice flags
            flag5 <- as.integer(substr(comb_flags, start = 5, stop = 5))
            dx2_list <- lapply(unique(flag4), function(i) {
                idx <- flag4 == i
                sub_flag5 <- flag5[idx]
                u_flag5 <- sort(unique(sub_flag5))
                idx_list <- lapply(u_flag5, function(a) {
                    which(sub_flag5 == a)
                })
                names(idx_list) <- as.character(u_flag5)
                call_list <- c(idx_list,
                               list(KEEP.OUT.ATTRS = FALSE,
                                    stringsAsFactors = FALSE)
                               )
                grid_mat <- as.matrix(do.call(expand.grid, call_list))
                sapply(seq_len(nrow(grid_mat)), function(ii) {
                    paste(comb_dx[idx][grid_mat[ii, ]], collapse = "+")
                })
            })
            dx2 <- paste(do.call(c, dx2_list), collapse = ",")
            if (length(dx1) > 0)
                paste(paste(dx1, collapse = ","), dx2, sep = ",")
            else
                dx2
        } else {
            paste(dx, collapse = ",")
        }
    } else if (length(dx) == 0L) {
        ""
    } else
        dx
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
         compress = TRUE, compression_level = 9, version = 2)
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
