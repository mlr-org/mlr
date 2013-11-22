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
	printf  "\nCleaning up ...\n"
	${DELETE} src/*.o src/*.so *.tar.gz
	${DELETE} html
	${DELETE} staticdocs
	${DELETE} *.Rcheck
	${DELETE} .RData .Rhistory

roxygenize: clean
	printf "\nRoxygenizing package ...\n"
	${RSCRIPT} ./tools/roxygenize

package: roxygenize
	printf "\nBuilding package file $(TARGZ)\n"
	${R} CMD build .

install: package
	printf "\nInstalling package $(TARGZ)\n"
	${R} CMD INSTALL $(TARGZ)

test: install
	printf "\nTesting package $(TARGZ)\n"
	${RSCRIPT} ./test_all.R

check: package
	printf "\nRunning R CMD check ...\n"
	${R} CMD check $(TARGZ)

check-rev-dep: package
	printf "\nRunning reverse dependency checks for CRAN ...\n"
	${RSCRIPT} ./tools/check-rev-dep

htmlhelp: install
	printf "\nGenerating html docs...\n"
	mkdir staticdocs
	${DELETE} /tmp/pkgdocs
	mkdir /tmp/pkgdocs
	mv README.md README.xxx
	${RSCRIPT} ./tools/generate-html-docs
	mv README.xxx README.md
	${DELETE} Rplots*.pdf
	git checkout gh-pages
	rm -rf *
	mv /tmp/pkgdocs/* .
	git add .
	git commit -am "new html help"
	git push origin gh-pages
	git checkout master


