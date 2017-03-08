#!/bin/sh

if [ -z "$1" ] ; then
  echo "Argument: files to change."
  exit 1
fi

sed -i '/^ *[^#]/s/,\([^ ]\)/, \1/g' "$@"
sed -i '/^ *[^#]/s/\([^,[#]\) \+,/\1,/g' "$@"
sed -i '/^ *[^#]/s/ \+(/(/g' "$@"
sed -i '/^ *[^#]/s/for(/for (/g' "$@"
sed -i '/^ *[^#]/s/while(/while (/g' "$@"
sed -i '/^ *[^#]/s/if(/if (/g' "$@"
sed -i '/^ *[^#]/s/\([^ a-zA-Z0-9#]\)(/\1 (/g' "$@"
sed -i "/^ *[^#\"][^\"]*$/s/'/\"/g" "$@"
sed -i '/^ *[^#]/s/^\([^#(]*\)\([^#<]\)<-/\1\2=/g' "$@"

