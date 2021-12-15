# Part 1: Replaces all level 2 headers and appends a ":" at the end of the line
# Part 2: Indents all bullet points with a whitespace
# Part 3: Removes all level 2 headers
# Part 4: For all level 1 headers, add linebreak and 80 hyphens (not strictly required but clean)
# Part 5: Remove all level 1 headers

sed -e '/^##/ s/$/:/' -e 's/^*/ */' -e 's/^## *//' -e "/^#/a\\--------------------------------------------------------------------------------" -e 's/^# *//' < NEWS.md > NEWS
