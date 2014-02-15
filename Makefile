include makeR/Makefile
UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
	SED_OPTION = -i
endif
ifeq ($(UNAME), Darwin)
	SED_OPTION = -i ''
endif


tutorial:
	echo  "Knitting Rmd tutorial..."
	${DELETE} doc/knitted
	${RSCRIPT} ./tools/generate-md-tutorial
	sed $(SED_OPTION) s/\`\`\`r/\`\`\`splus/ doc/knitted/*.md
	sed $(SED_OPTION) s/\`\`\`r/\`\`\`splus/ doc/knitted/tutorial/*.md


