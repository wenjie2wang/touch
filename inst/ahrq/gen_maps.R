#!/usr/bin/Rscript --vanilla


################################################################################
### This script generates the four mappings from ICD-9 (ICD-10) codes
### based on the CMS General Equivalence Mapping (GEM) from the map data
### 1. the GEM forward mapping from ICD-9 to ICD-10
### 2. the reverse of the forward mapping
### 3. the GEM backward mapping from ICD-10 to ICD-9
### 4. the reverse of the backward mapping
###
### version controlled by git
################################################################################


### set up tools
## just in case for using Rscript
library(methods)

## global settings
options(stringsAsFactors = FALSE)

## load helpers
source("../../R/utils.R")
source("../../R/format.R")

## using data.table now, may rewrite it to only use R base
if (! require(data.table))
    install.packages("data.table", repos = "https://could.r-project.org")
library(data.table)

## i/o
inDir <- "gem"
outDir <- "."

## gem of year 2017 and 2018
years <- c(2017, 2018)
forward_files <- sprintf("%d_I9gem.txt", years)
backward_files <- sprintf("%d_I10gem.txt", years)

for (i in seq_along(years)) {

    forward_file <- forward_files[i]
    backward_file <- backward_files[i]

    ## on the forward mapping
    forward_dat <- fread(file = file.path(inDir, forward_file), header = FALSE)
    colnames(forward_dat) <- c("icd9_codes", "icd10_codes", "flag")

    ## remove "NoDx"
    forward_dat[, sum(icd10_codes == "NoDx"), ]
    forward_dat[, sum(icd9_codes == "NoDx"), ]
    forward_dat <- forward_dat[icd10_codes != "NoDx" & icd9_codes != "NoDx", ]

    ## aggregate icd-10 codes for the same icd-9 code
    forward_agg_icd10 <-
        forward_dat[, .(icd10_codes = aggregate_dx(icd10_codes)),
                    keyby = icd9_codes]
    ## aggregate icd-9 codes for the same icd-10 code
    forward_agg_icd9 <-
        forward_dat[, .(icd9_codes = aggregate_dx(icd9_codes)),
                    keyby = icd10_codes]

    ## the forward mapping
    forward_map <- with(forward_agg_icd10,
                        dict(keys = paste0("icd9_", icd9_codes),
                             values = icd10_codes))
    forward_name <- sprintf("forward_map_%d", years[i])
    assign(forward_name, forward_map)

    ## the reverse of the forward mapping
    reverse_forward_map <- with(forward_agg_icd9,
                                dict(keys = paste0("icd10_", icd10_codes),
                                     values = icd9_codes))
    reverse_forward_name <- sprintf("reverse_forward_map_%d", years[i])
    assign(reverse_forward_name, reverse_forward_map)


    ## on the backward mapping
    backward_dat <- fread(file = file.path(inDir, backward_file),
                          header = FALSE)
    colnames(backward_dat) <- c("icd10_codes", "icd9_codes", "flag")

    ## remove "NoDx"
    backward_dat[, sum(icd9_codes == "NoDx"), ]
    backward_dat[, sum(icd10_codes == "NoDx"), ]
    backward_dat <- backward_dat[icd9_codes != "NoDx" & icd10_codes != "NoDx", ]

    ## aggregate icd-9 codes for the same icd-10 code
    backward_agg_icd9 <-
        backward_dat[, .(icd9_codes = aggregate_dx(icd9_codes)),
                     keyby = icd10_codes]
    ## aggregate icd-9 codes for the same icd-10 code
    backward_agg_icd10 <-
        backward_dat[, .(icd10_codes = aggregate_dx(icd10_codes)),
                     keyby = icd9_codes]

    ## the backward mapping
    backward_map <- with(backward_agg_icd9,
                         dict(keys = paste0("icd10_", icd10_codes),
                              values = icd9_codes))
    backward_name <- sprintf("backward_map_%d", years[i])
    assign(backward_name, backward_map)

    ## the reverse of the backward mapping
    reverse_backward_map <- with(backward_agg_icd10,
                                 dict(keys = paste0("icd9_", icd9_codes),
                                      values = icd10_codes))
    reverse_backward_name <- sprintf("reverse_backward_map_%d", years[i])
    assign(reverse_backward_name, reverse_backward_map)

    ## save all mappings into a RData file
    save(list = c(forward_name, reverse_forward_name,
                  backward_name, reverse_backward_name),
         file = file.path(outDir, sprintf("gem_%d.RData", years[i])))

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

## combine the current R/sysdata.rda with mapping environments
all_rda <- c("../../R/sysdata.rda", sprintf("gem_%d.RData", years))
all_rda_list <- as.list(all_rda)
do.call(combine_rda, c(all_rda_list, out_file = all_rda[1]))

## clean RData files
unlink(all_rda[- 1L], force = TRUE)
