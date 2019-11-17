objects := $(wildcard R/*.R) $(wildcard src/*.[hc]pp) DESCRIPTION
version := $(shell grep "Version" DESCRIPTION | awk '{print $$NF}')
pkg := $(shell grep "Package" DESCRIPTION | awk '{print $$NF}')
tar := $(pkg)_$(version).tar.gz
checkLog := $(pkg).Rcheck/00check.log
# tests := $(wildcard tests/testthat/*.R)

.PHONY: check
check: $(checkLog)

.PHONY: build
build: $(tar)

$(tar): $(objects)
	@$(RM) -rf src/RcppExports.cpp R/RcppExports.R
	@Rscript -e "library(methods);" \
	-e "Rcpp::compileAttributes()" \
	-e "devtools::document();";
	@$(MAKE) updateTimestamp
	R CMD build --resave-data .

$(checkLog): $(tar)
	R CMD check --as-cran $(tar)

.PHONY: install
install: $(tar)
	R CMD INSTALL $(tar)

# pkgdown
.PHONY: pkgdown
pkgdown:
	Rscript -e "library(methods); pkgdown::build_site();"

## update date in DESCRIPTION
.PHONY: updateTimestamp
updateTimestamp:
	@bash misc/update_timestamp.sh

## make tags
.PHONY: TAGS
TAGS:
	Rscript -e "utils::rtags(path = 'R', ofile = 'TAGS')"
	gtags

.PHONY: clean
clean:
	@rm -rf *~ */*~ *.Rhistroy *.tar.gz *.Rcheck/ .\#*

.PHONY: cleanCache
cleanCache:
	@rm -rf src/{*.o,*.so,RcppExports.cpp} R/RcppExports.R

.PHONY: cleanAll
cleanAll: clean cleanCache
