include makeR/Makefile

tutorial:
	printf "\nKnitting Rmd tutorial...\n"
	${DELETE} doc/knitted
	${DELETE} doc/figure
	mkdir doc/knitted
	mkdir doc/knitted/tutorial
	${RSCRIPT} ./tools/generate-md-tutorial
	mv doc/figure doc/knitted/tutorial/figure


