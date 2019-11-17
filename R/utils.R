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

### internal utility functions

## replace empty strings to NA_character
empty2na <- function(x) {
    x[x == ""] <- NA_character_
    x
}

## helper that checks whether the input character vector contains any comma
has_comma <- function(x) {
    any(grepl(",", x, fixed = TRUE))
}

## aggregate dx list to a tidy data.frame
dx_list2tidy <- function(x, x_names = NULL, col_names = NULL) {
    if (is.null(x_names)) {
        x_names <- names(x)
    }
    if (is.null(x_names)) {
        out <- do.call(rbind, lapply(x, function(a) {
            data.frame(a, stringsAsFactors = FALSE)
        }))
    } else {
        out <- do.call(rbind, lapply(seq_along(x), function(i) {
            data.frame(x_names[i], x[[i]], stringsAsFactors = FALSE)
        }))
    }
    ## add colnames if needed
    if (! is.null(col_names))
        colnames(out) <- col_names
    ## return out
    out
}
