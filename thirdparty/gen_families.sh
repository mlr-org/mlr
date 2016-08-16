#!/bin/sh
echo -E '\name{mlrFamilies}'
echo -E '\title{mlr documentation families}'
echo -E '\description{List of all mlr documentation families with members.}'
echo -E '\arguments{'
awk 'BEGIN { fam = 0 } /@family/ { fam = $3 } /^[a-z]/ { if(fam) { fams[fam] = fams[fam] $1 ", "; fam = 0 } } END { for(fam in fams) { sub(/, $/, "", fams[fam]); print "\\item{" fam "}{" fams[fam] "}" } }' R/*
echo '}'
