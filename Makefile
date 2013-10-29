R	:= R --no-save --no-restore
RSCRIPT	:= Rscript
DELETE	:= rm -fR
VERSION := $(shell Rscript ./tools/get-version)
TARGZ   := mlr_$(VERSION).tar.gz

.SILENT:
.PHONEY: clean roxygenize package windows install test check

usage:
	echo "Available targets:"
	echo ""
	echo " clean         - Clean everything up"
	echo " roxygenize    - roxygenize in-place"
	echo " package       - build source package"
	echo " install       - install the package"
	echo " test          - run unit tests"
	echo " check         - run R CMD check on the package"
	echo " html          - build static html documentation"


clean:
	echo -e "\nCleaning up ..."
	${DELETE} src/*.o src/*.so *.tar.gz
	${DELETE} html
	${DELETE} *.Rcheck
	${DELETE} .RData .Rhistory

roxygenize: clean
	echo -e "\nRoxygenizing package ..."
	${RSCRIPT} ./tools/roxygenize

package: roxygenize
	echo -e "\nBuilding package file $(TARGZ)"
	${R} CMD build . 
 
install: package
	echo -e "\nInstalling package $(TARGZ)"
	${R} CMD INSTALL $(TARGZ) 

test: install
	echo -e "\nTesting package $(TARGZ)"
	${RSCRIPT} ./test_all.R

check: package
	echo -e "\nRunning R CMD check ..."
	${R} CMD check $(TARGZ)

check-rev-dep: package
	echo -e "\nRunning reverse dependency checks for CRAN ..."
	${RSCRIPT} ./tools/check-rev-dep

html: install
	echo -e "\nGenerating html docs..."
	${DELETE} html
	mkdir html
	${RSCRIPT} ./tools/generate-html-docs
  
