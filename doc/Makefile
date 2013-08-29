SPHINXBUILD   := sphinx-build
WWWDIR        := ../www

RNWFILES  = $(wildcard *.Rnw)
RNWFILES += $(wildcard tutorial/*.Rnw)
RNWFILES += $(wildcard extension_points/*.Rnw)
RSTFILES  = $(RNWFILES:.Rnw=.rst)

.PHONY: help clean html linkcheck Rnw2rst

help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  html       to make standalone HTML files"
	@echo "  Rnw2rst    to make the rst files from the Rnw input files"
	@echo "  linkcheck  to check all cross-references / links"

clean:
	@echo "Cleaning up ..." 
	@-rm -f $(WWWDIR)/*.html 
	@-rm -f $(WWWDIR)/tutorial/*.html
	@-rm -f $(RSTFILES) $(RNWFILES:.Rnw=.log) 
	@-rm -f _linkcheck/

html: clean Rnw2rst
	$(SPHINXBUILD) -b html . $(WWWDIR)
	@echo
	@echo "Build finished. The HTML pages are in $(WWWDIR)."


linkcheck: Rnw2rst
	$(SPHINXBUILD) -b linkcheck . _linkcheck
	@echo
	@echo "Link check complete; look for any errors in the above output " \
	      "or in _linkcheck/output.txt."


Rnw2rst: $(RSTFILES)

%.rst: %.Rnw
	@echo "Building '$@' from '$<' ..."
	@Rscript --vanilla -e "library(\"ascii\"); Sweave(\"$<\", output=\"$@\", driver=RweaveReST)" > /dev/null 2> $(<:.Rnw=.log)

