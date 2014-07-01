include makeR/Makefile
MAN_URL := http://berndbischl.github.io/mlr/man/
UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
	SED_OPTION = -i
endif
ifeq ($(UNAME), Darwin)
	SED_OPTION = -i ''
endif


tutorial:
	# echo  "Knitting Rmd tutorial..."
	# copy all crap to temp dir
	${DELETE} doc/temp
	mkdir doc/temp
	cp doc/*.Rmd doc/temp
	cp -r doc/tutorial doc/temp/tutorial
	# now substitute our man and man2 macros
	sed $(SED_OPTION) 's,\\man\[\([a-zA-Z]*\)\],'"[\1]($(MAN_URL)\1.html)"',' doc/temp/tutorial/*.Rmd
	sed $(SED_OPTION) 's,\\man2\[\([a-zA-Z]*\)\]\[\([a-zA-Z]*\)\],'"[\1]($(MAN_URL)\2.html)"',' doc/temp/tutorial/*.Rmd
	# generate results into doc/knitted
	${DELETE} doc/knitted
	${RSCRIPT} ./tools/generate-md-tutorial
	# splus looks nicer
	sed $(SED_OPTION) s/\`\`\`r/\`\`\`splus/ doc/knitted/*.md
	sed $(SED_OPTION) s/\`\`\`r/\`\`\`splus/ doc/knitted/tutorial/*.md
	# delete crap
	${DELETE} doc/temp


