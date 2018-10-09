.icd9tab <- read.table("icd.txt", sep=":", as.is=TRUE)
.drg29tab <- read.table("drg.txt", sep=":", as.is=TRUE)
.drgspecial <- read.table("htncx_renlfail.txt", sep=":", as.is=TRUE)

## creat a function to match codes
codeMatch.1 <- function(codes) {
  codes <- gsub("E", "11", codes) ## gsub instead of sub!
  codes <- gsub("V", "12", codes)
  if (codes == "") return(function(x) rep(FALSE, length(x)))
  ll <- strsplit(codes, ",")[[1]]
  isRange <- grepl("-", ll)
  textV <- textR <- ""
  if (any(!isRange)) {
    vc <- ll[!isRange]
    textV <- paste("(x %in% c(", paste(vc, collapse=", "), "))")
  }
  if (any(isRange)) {
    ran <- t(as.data.frame(lapply(strsplit(ll[isRange], "-"), as.numeric)))
    textR <- paste("(", "x >= ", ran[,1], " & ", "x <= ", ran[,2], ")")
    textR <- paste(textR, collapse = " | ")
  }
  both <- length(table(isRange)) > 1
  sep <- if (both) " | " else ""
  text <- paste(textV, textR, sep=sep)
  fun <- function(x) {}
  body(fun) <- parse(text=text)
  fun
}

# input htncx_renlfail.txt and create function
# to compute the drg flags for 2 comorbidities
# HTNCX and RENLFAIL with carddrg and renaldrg
specialDrg.2 <- function(codes) {
  ll <- strsplit(codes, ",")[[1]]
  ll <- trimws(ll)
  text <- ""
  text <- paste0("with(icd, rowSums(cbind(", paste(ll, collapse=", "), ")))")
  fun <- function(icd, CARDDRG, RENALDRG) {}
  body(fun) <- parse(text=text)
  fun
}

## a list of functions for icd9 codes
cmbdFuns <- sapply(1:nrow(.icd9tab),
                   function(i) codeMatch.1(.icd9tab[i,2])
                   )

names(cmbdFuns) <- .icd9tab[,1]

## a list of functions for drg code
drgFuns <- sapply(1:nrow(.drg29tab),
                  function(i) codeMatch.1(.drg29tab[i,2])
                  )
names(drgFuns) <- .drg29tab[,1]

## a list of functions for handling drg flags for two comorbidities
specdrgFuns <- sapply(1:nrow(.drgspecial),
                  function(i) specialDrg.2(.drgspecial[i,2])
)

names(specdrgFuns) <- .drgspecial[,1]

save(cmbdFuns, drgFuns, specdrgFuns,
     file='../../R/sysdata.rda', compress=TRUE)
