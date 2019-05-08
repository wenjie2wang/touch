# R Package touch

[![CRAN_Status_Badge][cranVersion]][cran]
[![Build Status][travis-master]][travis]


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

You may install the latest version under development with the help of
**remotes** (or **devtools**) as follows:

```R
if (! require(remotes)) install.packages("remotes")
remotes::install_github("wenjie2wang/touch")
## or use devtools if it is installed
## devtools::install_github("wenjie2wang/touch")
```


## License

The R package touch is free software: You can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or any later version (at
your option).  See the [GNU General Public License][gpl] for details.

The R package touch is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.


[cranVersion]: https://www.r-pkg.org/badges/version/touch
[cran]: https://CRAN.R-project.org/package=touch
[travis]: https://travis-ci.org/wenjie2wang/touch
[travis-master]: https://travis-ci.org/wenjie2wang/touch.svg?branch=master
[gpl]: https://www.gnu.org/licenses/
