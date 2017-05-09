clean: restorename cleanman
restoreman:
	git checkout man
restorename:
	git checkout NAMESPACE;git checkout DESCRIPTION
cleanman: restoreman
	git clean -f man
