include makeR/Makefile
UNAME := $(shell uname)
ifeq (($UNAME), Linux)
	SED_OPTION = -i
endif
ifeq (($UNAME), Darwin)
	SED_OPTION = -i ''
endif

tutorial:
	printf "\nKnitting Rmd tutorial...\n"
	${DELETE} doc/knitted
	${DELETE} doc/figure
	mkdir doc/knitted
	mkdir doc/knitted/tutorial
	${RSCRIPT} ./tools/generate-md-tutorial
	sed $(SED_OPTION) s/\`\`\`r/\`\`\`splus/ *.md
	mv doc/figure doc/knitted/tutorial/figure


