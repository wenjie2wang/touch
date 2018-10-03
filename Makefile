objects := $(wildcard R/*.R) DESCRIPTION
version := $(shell grep "Version" DESCRIPTION | awk '{print $$NF}')
pkg := $(shell grep "Package" DESCRIPTION | awk '{print $$NF}')
tar := $(pkg)_$(version).tar.gz
checkLog := $(pkg).Rcheck/00check.log
# tests := $(wildcard tests/testthat/*.R)
# rmd := vignettes/$(pkg)-intro.Rmd
# vignettes := vignettes/$(pkg)-intro.html


.PHONY: check
check: $(checkLog)

.PHONY: build
build: $(tar)

# .PHONY: preview
# preview: $(vignettes)

$(tar): $(objects)
	@$(MAKE) -s updateTimestamp
	Rscript -e "library(methods); devtools::document();";
	R CMD build --resave-data .

$(checkLog): $(tar)
	R CMD check --as-cran $(tar)

.PHONY: install
install: $(tar)
	R CMD INSTALL $(tar)

## update date in DESCRIPTION
.PHONY: updateTimestamp
updateTimestamp:
	dt=$$(date +"%Y-%m-%d");\
	sed -i "s/Date: [0-9]\{4\}-[0-9]\{1,2\}-[0-9]\{1,2\}/Date: $$dt/" DESCRIPTION;

## make tags
.PHONY: TAGS
TAGS:
	Rscript -e "utils::rtags(path = 'R', ofile = 'TAGS')"

.PHONY: clean
clean:
	@rm -rf *~ */*~ *.Rhistroy *.tar.gz *.Rcheck/ .\#*

.PHONY: cleanCache
cleanCache:
	@rm -rf src/{*.o,*.so,RcppExports.cpp} R/RcppExports.R

.PHONY: cleanAll
cleanAll: clean cleanCache
