### internal utility functions
## create an environment from a given set of keys and values
dict <- function(keys, values)
{
    res <- new.env(hash = TRUE, parent = emptyenv())
    ## convert keys to character strings if needed
    if (! is.character(keys))
        keys <- as.character(keys)
    ## check if any duplicated keys
    if (anyDuplicated(keys)) {
        lastIdx <- ! duplicated(keys, fromLast = TRUE)
        keys <- keys[lastIdx]
        values <- values[lastIdx]
        warning("Values for duplicated keys will be overwritten",
                "and only the last value will be used.")
    }
    ## recycle values if needed
    len_keys <- length(keys)
    if (len_keys != length(values))
        values <- rep_len(values, length.out = len_keys)
    ## assign values to keys in res
    for (i in seq_along(keys)) {
        assign(keys[i], values[i], res)
    }
    ## return
    res
}

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
