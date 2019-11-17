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
