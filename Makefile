pkg = touch

Rpkg: build
	make check

Rd: R/
	Rscript -e "library(methods); devtools::document();" # roxygen2::roxygenise();"

build: Rd
	R CMD build ../$(pkg)

check: $(pkg)_*.tar.gz
	R CMD check --as-cran $(pkg)_*.tar.gz

INSTALL: $(pkg)_*.tar.gz
	R CMD INSTALL --build $(pkg)_*.tar.gz

clean:
	rm -rf *~ */*~ *.Rhistroy *.tar.gz *.Rcheck/ .\#*
