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
