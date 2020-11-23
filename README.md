# R Package touch

[![CRAN_Status_Badge][cranVersion]][cran]
[![Build Status][gha-icon]][gha-url]


The package **touch** (**T**ools **o**f **u**nilization and **c**ost in
**h**ealthcare) provides **R** implementation of the software tools developed
in the H-CUP (Healthcare Cost and Utilization Project) and AHRQ (Agency for
Healthcare Research and Quality).


It currently contains functions for

- mapping ICD-9 codes to the AHRQ comorbidity measures
- translating ICD-9 (resp. ICD-10) codes to ICD-10 (resp. ICD-9) codes based
  on GEM (General Equivalence Mappings) from CMS (Centers for Medicare and
  Medicaid Services).


## Installation

You may install the released version from [CRAN][cran].

```R
install.packages("touch")
```


## Development

One may install the latest version under development as follows:

```R
if (! require(remotes)) install.packages("remotes")
remotes::install_github("wenjie2wang/touch", upgrade = "never")
```

## License

[GNU General Public License][gpl] (≥ 3)


[cranVersion]: https://www.r-pkg.org/badges/version/touch
[cran]: https://CRAN.R-project.org/package=touch
[gha-icon]: https://github.com/wenjie2wang/touch/workflows/R-CMD-check/badge.svg
[gha-url]: https://github.com/wenjie2wang/touch/actions
[gpl]: https://www.gnu.org/licenses/
