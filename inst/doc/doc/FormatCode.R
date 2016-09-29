library(stringr)
# read lines in txt file
url <- "http://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comformat2011.txt"
format.txt <- readLines(url)
# delete all the SAS annotation and code rows, keep the ICD-9-CM and DRG codes
format.txt <- str_trim(grep(pattern="\\d|=|VALUE", x=format.txt, value=TRUE))
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
  DRG.class <- str_trim(DRG.class)
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
DRG.code <- str_trim(DRG.code)
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
ICD.class <- str_trim(substr(ICD.name, start= eq.order+1, stop=a.order-1))
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

ICD.code <- str_trim(substr(ICD.code, start= 1, stop=I-1))
ICD.code <- str_replace_all(ICD.code, pattern=' ', replacement="")
ICD.code <- str_replace_all(ICD.code, pattern=',', replacement=", ")
ICD.code <- str_replace_all(ICD.code, pattern='\"', replacement="")

# DRG.code: DRG codes for temporary 24 DRG group
# DRG.class: DRG name for temporary 24 DRG group
# ICD.code: ICD-9 codes for temporary 40 comorbidity group
# ICD.class: ICD-9 name for temporary 40 comorbidity group

drg <- as.list(DRG.code)
names(drg) <- DRG.class

# according to DRG_combination.txt, generate a final drg.txt like the txt in Git
drg.comb <- readLines('C:/Users/Leonhard/Desktop/touch/DRG_combination.txt')
seprt <- subset(str_locate(drg.comb, pattern="="), select=start)
cmbd.name <- str_trim(substr(drg.comb, start=1, stop=seprt-1))
cmbd.comb <- str_trim(substr(drg.comb, start=seprt+1, stop=nchar(drg.comb)))

cmbd.drg <- list()
for(i in 1:length(cmbd.comb)) {
  m <- str_trim(str_split_fixed(cmbd.comb[i], pattern=",", 
                  n=str_count(cmbd.comb[i], pattern=',')+1))
  cmbd.drg[[i]] <- m
}
names(cmbd.drg) <- cmbd.name

for(i in 1:length(cmbd.drg)) {
  cmbd.i <- ""
  for(j in 1:length(cmbd.drg[[i]])) {
    num <- which(names(drg)==cmbd.drg[[i]][j])
    if(cmbd.i == "") {
      cmbd.i <- str_c(cmbd.i, drg[[num]], sep="")
    } else {
      cmbd.i <- str_c(cmbd.i, drg[[num]], sep=", ")
    }
  }
  cmbd.drg[[i]] <- cmbd.i
}

# change abbreviation of names to labels
label <- readLines('C:/Users/Leonhard/Desktop/touch/labels.txt')
seprt <- subset(str_locate(label, pattern="="), select=start)
label.name <- str_trim(substr(label, start=1, stop=seprt-1))
label.annote <- str_trim(substr(label, start=seprt+1, stop=nchar(label)))

f.drg <- NULL
for(i in 1:length(cmbd.drg)) {  
  num <- which(label.name == names(cmbd.drg[i]))
  f.drg <- c(f.drg, paste0(i,". ", label.annote[num], ": ", cmbd.drg[[i]]))
}

f.drg <- str_replace_all(f.drg, pattern="'", replacement="")
cat(f.drg, file="drg.txt", sep="\n")
