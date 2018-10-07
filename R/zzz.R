## specify some imports for the package
##' @importFrom Rcpp sourceCpp
##' @useDynLib touch
NULL


.onLoad <- function(libname, pkgname)
{
    ## initialize package cache in cpp
    ## year 2017
    with(forward_map_2017,
         init_gem_f17(icd9_codes, icd10_codes))
    with(backward_map_2017,
         init_gem_b17(icd10_codes, icd9_codes))
    with(reverse_forward_map_2017,
         init_gem_rf17(icd10_codes, icd9_codes))
    with(reverse_backward_map_2017,
         init_gem_rb17(icd9_codes, icd10_codes))

    ## year 2018
    with(forward_map_2018,
         init_gem_f18(icd9_codes, icd10_codes))
    with(backward_map_2018,
         init_gem_b18(icd10_codes, icd9_codes))
    with(reverse_forward_map_2018,
         init_gem_rf18(icd10_codes, icd9_codes))
    with(reverse_backward_map_2018,
         init_gem_rb18(icd9_codes, icd10_codes))

    ## return NULL invisiably
    invisible()
}
