#!/bin/sh

if [ -z "$1" ] ; then
  echo "Argument: files to change."
  exit 1
fi
# this has a few false positives, especially within string constants.

sed -i '/^ *[^ #]/s/,\([^ ]\)/, \1/g' "$@"
sed -i '/^ *[^ #]/s/,\([^ ]\)/, \1/g' "$@" # do this multiple times to catch [,,1] etc
sed -i '/^ *[^ #]/s/,\([^ ]\)/, \1/g' "$@"
sed -i '/^ *[^ #]/s/\([^,[#]\) \+,/\1,/g' "$@"
sed -i '/^ *[^ #]/s/ \+(/(/g' "$@"
sed -i '/^ *[^ #]/s/\([^a-zA-Z._0-9]\)for(/\1for (/g' "$@"
sed -i '/^ *[^ #]/s/\([^a-zA-Z._0-9]\)while(/\1while (/g' "$@"
sed -i '/^ *[^ #]/s/\([^a-zA-Z._0-9]\)if(/\1if (/g' "$@"
sed -i '/^ *[^ #]/s/\([^a-zA-Z._0-9]\)in(/\1in (/g' "$@"
sed -i '/^ *[^ #]/s/\([^ !a-zA-Z0-9#:([]\)(/\1 (/g' "$@"
sed -i "/^ *[^ #\"][^\"]*$/s/'/\"/g" "$@"
sed -i '/^ *[^ #]/s/^\([^#(]*\)\([^#<]\)<-/\1\2=/g' "$@"

