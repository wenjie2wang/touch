library(touch)

### some random ICD-9 and ICD-10 codes
icd9codes <- c("0011", "001.1", "316", "29383", "E9808", "V90")
icd10codes <- c("F0390", "F0630", "F54", "F30.13", "A010", "M61019")

### forward mapping from ICD-9 to ICD-10
icd_map(icd9codes)
icd_map(icd9codes, decimal = TRUE, nomatch = NA)

### backward mapping from ICD-10 to ICD-9
icd_map(icd10codes, from = 10, to = 9)
icd_map(icd10codes, from = 10, to = 9, nomatch = NA, output = "list")
icd_map(icd10codes, from = 10, to = 9,
        decimal = TRUE, nomatch = NA, output = "tidy")

### reverse-backward mapping from ICD-9 to ICD-10
icd_map(icd9codes, method = "reverse-gem")
icd_map(icd9codes, method = "reverse", decimal = TRUE, nomatch = NA)

### reverse-forward mapping from ICD-10 to ICD-9
icd_map(icd10codes, from = 10, to = 9, method = "reverse-gem")
icd_map(icd10codes, from = 10, to = 9, method = "reverse",
        decimal = TRUE, nomatch = NA)

### forward and reverse-backward mapping from ICD-9 to ICD-10
icd_map(icd9codes, method = "both")
icd_map(icd9codes, method = "both", decimal = TRUE, nomatch = NA)

### backward and reverse-forward mapping from ICD-10 to ICD-9
icd_map(icd10codes, from = 10, to = 9, method = "both")
icd_map(icd10codes, from = 10, to = 9, method = "both",
        decimal = TRUE, nomatch = NA)

### multi-stage process mapping ICD-9 to ICD-10
icd_map(icd9codes, method = "multi-stage")
icd_map(icd9codes, method = "multi-stage", decimal = TRUE, nomatch = NA)

### multi-stage process mapping ICD-10 to ICD-9
icd_map(icd10codes, from = 10, to = 9,
        method = "multi-stage", cache = FALSE)
icd_map(icd10codes, from = 10, to = 9, method = "multi-stage",
        decimal = TRUE, nomatch = NA, cache = FALSE)

### For codes with positive combination flags
icd_map("24951", output = "list")
## where the "+" signs indicate the code combinations
