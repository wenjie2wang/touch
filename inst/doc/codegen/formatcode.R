CatchCode <- function(x) {
  # Arg:
  #  x: url of SAS format code
  # output:
  #  DRG.code: DRG codes for temporary 24 DRG group
  #  DRG.class: DRG name for temporary 24 DRG group
  #  ICD.code: ICD-9 codes for temporary 40 comorbidity group
  #  ICD.class: ICD-9 name for temporary 40 comorbidity group
  format.txt <- readLines(url)
  # delete all the SAS annotation and code rows, keep the ICD-9-CM and DRG codes
  format.txt <- trimws(grep(pattern="\\d|=|VALUE", x=format.txt, value=TRUE))
  format.txt <- format.txt[which(substr(format.txt, start=1, stop=1) != "/")]
  Fstart <- format.txt[which(substr(format.txt, start=1, stop=1) == '"')][1]
  format.txt <- format.txt[-(1 : which(format.txt == Fstart) - 1)]
  # select rows that contains the names of DRG group
  DRG.name <- grep(pattern="VALUE", x=format.txt, value=TRUE)
  DRG.order <- NULL
  for (i in 1:length(DRG.name)) {
    DRG.order <- c(DRG.order, which(format.txt == DRG.name[i]))
  }

  DRG.class <- NULL
  # store the names of DRG in DRG.class array using the names like CARDDRG
  for (i in 1 : length(DRG.name)) {
    start.flag <- str_locate(DRG.name[i], pattern='(DRG)')[[1]]
    DRG.class <- c(DRG.class, substr(DRG.name[i],
                                     start=6,
                                     stop=start.flag[length(start.flag)]+2))
    DRG.class <- trimws(DRG.class)
  }

  DRG.code <- NULL
  # by Group catenate all of the strings containing the DRG codes
  for (i in 1 : (length(DRG.order))) {
    DRG.code.k <- " "
    if(i!=length(DRG.order)) {
      for(k in (DRG.order[i] + 1) : (DRG.order[i + 1] - 1)) {
        DRG.code.k <- str_c(DRG.code.k, format.txt[k], sep="")
      }
    } else {
      for(k in (DRG.order[i] + 1) : length(format.txt)) {
        DRG.code.k <- str_c(DRG.code.k, format.txt[k], sep="")
      }
    }
    DRG.code <-c(DRG.code, DRG.code.k)
  }
  # remove the SAS annotation in DRG.code
  eq.start <- subset(str_locate(DRG.code, pattern="="), select=start)
  DRG.code <- substr(DRG.code, start=1, stop=eq.start-1)
  DRG.code <- trimws(DRG.code)
  DRG.code <- str_replace_all(DRG.code, pattern=' ', replacement="")
  DRG.code <- str_replace_all(DRG.code, pattern=',', replacement=", ")

  # handling ICD-9 codes
  ICD <- format.txt[1 : (DRG.order[1]-2)]

  ICD.name <- grep(pattern="=", x=ICD, value=TRUE)

  ICD.order <- NULL ## the numbers of row containing comorbodity names.
  for (i in 1:length(ICD.name)) {
    ICD.order <- c(ICD.order, which(format.txt == ICD.name[i]))
  }
  # select comorbodity nanmes and store in ICD.name
  eq.order <- subset(str_locate(ICD.name, pattern="="), select=start)
  a.order <- subset(str_locate(ICD.name, pattern="(/)"), select=start)
  ICD.class <- trimws(substr(ICD.name, start= eq.order+1, stop=a.order-1))
  ICD.class <- str_replace_all(ICD.class, pattern='\"', replacement="")

  ICD.code <- NULL
  for (i in 1 : (length(ICD.order))) {
    ICD.code.k <- " "
    if(i!=1) {
      for(k in (ICD.order[i-1] + 1) : (ICD.order[i])) {
        ICD.code.k <- str_c(ICD.code.k, format.txt[k], sep="")
      }
    } else {
      for(k in 1 : ICD.order[i]) {
        ICD.code.k <- str_c(ICD.code.k, format.txt[k], sep="")
      }
    }
    ICD.code <-c(ICD.code, ICD.code.k)
  }
  I <- subset(str_locate(ICD.code, pattern="="), select=start)

  ICD.code <- trimws(substr(ICD.code, start= 1, stop=I-1))
  ICD.code <- str_replace_all(ICD.code, pattern=' ', replacement="")
  ICD.code <- str_replace_all(ICD.code, pattern=',', replacement=", ")
  ICD.code <- str_replace_all(ICD.code, pattern='\"', replacement="")
  list(DRG.code=DRG.code, DRG.class=DRG.class,
       ICD.code=ICD.code, ICD.class=ICD.class)
}

Adddot <- function(input) {
  # Change the codes from SAS by adding dot after third digit
  # Inputs:
  #  ICD-9 codes got from SAS format code
  # Outputs:
  #  ICD-9 codes with three integers and two decimals
  ll <- input
  ll <- strsplit(ll, ",")[[1]]
  aa <- ";"
  for(k in 1:length(ll)) {
    ll[k] <- trimws(ll[k])
    isE <- grepl("E", ll[k])
    isV <- grepl("V", ll[k])
    isRange <- grepl("-", ll[k])
    if(!isRange) {
      nc <- nchar(ll[k])
      ll[k] <- gsub("E", "11", ll[k])
      ll[k] <- gsub("V", "12", ll[k])
      ll[k] <- ifelse(nc == 3, paste(ll[k], "00", sep=""), ll[k])
      ll[k] <- ifelse(nc == 4, paste(ll[k], "0", sep=""), ll[k])
      ll[k] <- as.numeric(ll[k]) / 100
      ll[k] <- ifelse(isE, paste("E", substr(ll[k], start=3,
                                          stop=nchar(ll[k])), sep=""), ll[k])
      ll[k] <- ifelse(isV, paste("V", substr(ll[k], start=3,
                                          stop=nchar(ll[k])), sep=""), ll[k])
    } else {
      inter <- trimws(strsplit(ll[[k]], "-")[[1]])
      inter <- sapply(inter,
                      function(x) {
                        x <- ifelse(nchar(x) == 4, paste(x, "0", sep=""), x)
                             ifelse(nchar(x) == 3, paste(x, "00", sep=""), x)
                      })
      inter <- gsub("E", "11", inter)
      inter <- gsub("V", "12", inter)
      inter <- sapply(inter, function(x) {as.numeric(x) / 100})
      inter <- sapply(inter,
                      function(x) {ifelse(isE, paste("E", substr(x, start=3,
                                             stop=nchar(x)), sep=""), x)})
      inter <- sapply(inter,
                      function(x) {ifelse(isV, paste("V", substr(x, start=3,
                                             stop=nchar(x)), sep=""), x)})
      ll[k] <- str_c(inter[1], inter[2], sep="-")
    }
  aa <- str_c(aa, ll[k], sep=", ")
  }
  sub("(;, )", "", aa)
}

MatchCode <- function(comb, code) {
  # match icd or drg comb with icd or drg code
  # args:
  #  comb: icd.comb or drg.comb
  #  code: icd or drg code
  seprt <- subset(str_locate(comb, pattern="="), select=start)
  cmbd.name <- trimws(substr(comb, start=1, stop=seprt-1))
  cmbd.comb <- trimws(substr(comb, start=seprt+1, stop=nchar(comb)))

  cmbd <- list()
  for(i in 1:length(cmbd.comb)) {
    m <- trimws(str_split_fixed(cmbd.comb[i], pattern=",",
                                  n=str_count(cmbd.comb[i], pattern=',')+1))
    cmbd[[i]] <- m
  }
  names(cmbd) <- cmbd.name
  for(i in 1:length(cmbd)) {
    cmbd.i <- ""
    for(j in 1:length(cmbd[[i]])) {
      num <- which(names(code)==cmbd[[i]][j])
      if(cmbd.i == "") {
        cmbd.i <- str_c(cmbd.i, code[[num]], sep="")
      } else {
        cmbd.i <- str_c(cmbd.i, code[[num]], sep=", ")
      }
    }
    cmbd[[i]] <- cmbd.i
  }
  cmbd
}

Matchnames <- function(code) {
  # change list of code to charactor vectors
  f <- NULL
  name <- names(code)
  for(i in 1:length(code)) {
    f <- c(f, paste0(name[i], ": ", code[[i]]))
  }
  str_replace_all(f, pattern="'", replacement="")
}

url <- read.table("sas-url.txt")[[6]]
url <- as.character(url)

cmbd <- CatchCode(url)

ICD.code <- cmbd$ICD.code
ICD.class <- cmbd$ICD.class
DRG.code <- cmbd$DRG.code
DRG.class <- cmbd$DRG.class

drg <- as.list(DRG.code)
names(drg) <- DRG.class
icd <- as.list(ICD.code)
icd <- lapply(icd, Adddot)
names(icd) <- ICD.class

# according to DRG_combination.txt, generate a final drg.txt like the txt in Git
drg.comb <- readLines('drg-combination.txt')
cmbd.drg <- MatchCode(drg.comb, drg)

# according to icd_combination.txt, generate a final drg.txt like the txt in Git
icd.comb <- readLines('icd-combination.txt')
cmbd.icd <- MatchCode(icd.comb, icd)

f.drg <- Matchnames(cmbd.drg)
f.icd <- Matchnames(cmbd.icd)

cat(f.drg, file="../drg.txt", sep="\n")
cat(f.icd, file="../icd.txt", sep="\n")
