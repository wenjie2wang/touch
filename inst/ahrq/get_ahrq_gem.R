#!/usr/bin/Rscript --vanilla


### This script simply downloads General Equivalence Mappings (GEM) from AHRQ
base_url <- "https://www.cms.gov/Medicare/Coding/ICD10/Downloads"

## GEM for year 2017
gem17_zip <- "2017-GEM-DC.zip"

## GEM for year 2018
gem18_zip <- "2018-ICD-10-CM-General-Equivalence-Mappings.zip"

## define output directory
outDir <- "downloads"
if (! dir.exists(outDir)) dir.create(outDir)

## download all
gem_zips <- c(gem17_zip, gem18_zip)
for (i in gem_zips) {
    outFile <- file.path(outDir, i)
    download.file(file.path(base_url, i), destfile = outFile)
    unzip(outFile, exdir = outDir)
    unlink(outFile)
    unlink(file.path(outDir, "*.pdf"), force = TRUE)
}

