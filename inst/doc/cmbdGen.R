.icd9tab <- read.table("cmbd.csv", sep=":", as.is=TRUE)
.drg29tab <- read.table("drg.txt", sep=":", as.is=TRUE)

source("../../R/cmbd.R")

## a list of functions for icd9 codes
cmbdFuns <- sapply(1:nrow(.icd9tab),
                   function(i) codeMatch.1(.icd9tab[i,2])
                   )
  
names(cmbdFuns) <- .icd9tab[,1]

## a list of functions for drg code
drgFuns <- sapply(1:nrow(.drg29tab),
                  function(i) codeMatch.1(.drg29tab[i,2])
                  )


save(.icd9tab, .drg29tab,
     cmbdFuns, drgFuns,
     file='sysdata.rda', compress=TRUE)
## resaveRdafile('sysdata.rda')


## icd9sample <- read.csv("icd9sample.csv", as.is=TRUE)
## save(icd9sample, file="icd9sample.rda", compress=TRUE)
