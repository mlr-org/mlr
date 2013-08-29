R  := R --no-save --no-restore
RSCRIPT	:= Rscript 
DELETE	:= rm -fR

.SILENT:
.PHONEY: clean roxygenize package dependencies install test html check check-rds

usage:
	echo "Available targets:"
	echo ""
	echo " clean         - Clean everything up"
	echo " roxygenize    - roxygenize skel/ into pkg/"
	echo " package       - build source package"
	echo " dependencies  - install required packages"
	echo " install       - install the package"
	echo " test          - run tests"
	echo " check         - run R CMD check on the package"
	echo " html          - build static html documentation"

clean:
	echo "Cleaning up ..."
	${DELETE} skel/src/*.o skel/src/*.so pkg.Rcheck
	${DELETE} pkg
	${DELETE} html
	${DELETE} .RData .Rhistory

roxygenize: clean
	echo "Roxygenizing package ..."
	${RSCRIPT} ../tools/roxygenize
	echo "Setting version ..."
	${RSCRIPT} ../tools/set-version

package: roxygenize
	echo "Building package file ..."
	${R} CMD build pkg/

dependencies:
	echo "Installing depencies"
	${RSCRIPT} install_dependencies.R 

install: roxygenize
	echo "Installing package ..."
	${R} CMD INSTALL pkg

test: install
	echo "Testing package ..."
	${RSCRIPT} ./test_all.R

check: roxygenize
	echo "Running R CMD check ..."
	${R} CMD check --as-cran pkg

check-rds: roxygenize
	echo "Checking each RD file individually ..."
	${RSCRIPT} ../tools/check-rds
  
html: install
	echo "Generating html docs..."
	${DELETE} html
	mkdir html
	${RSCRIPT} ../tools/generate-html-docs
   
  
  
