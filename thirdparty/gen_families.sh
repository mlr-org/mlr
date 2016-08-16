#!/bin/sh
echo '\\name{mlrFamilies}'
echo '\\title{mlr documentation families}'
echo '\\description{List of all mlr documentation families with members.}'
echo '\\arguments{'
awk 'BEGIN { fam = 0 } /@family/ { fam = $3 } /^[a-z]/ { if(fam) { fams[fam] = fams[fam] $1 ", "; fam = 0 } } END { for(fam in fams) { sub(/, $/, "", fams[fam]); print "\\item{" fam "}{" fams[fam] "}" } }' R/*
echo '}'
