# Tutorial
URL: http://berndbischl.github.io/mlr/tutorial/html/

## Howto
* Install dependencies:
  `pip install --user mkdocs` or `easy_install --user mkdocs`.
  Install R dependencies as required.
* Only edit R markdown files in subfolder `src`.
* Markdown basics:
  * Basic Markdown: https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet
  * RStudio Support: https://support.rstudio.com/hc/en-us/articles/200552086-Using-R-Markdown
  * Knitr options: http://yihui.name/knitr/options
* Put additional images in `../images`.
* Link to mlr manual: `[&function]` and `[name](&function)`.
* Link to other manuals: `[&pkg::function]` and `[name](&pkg::function)`.
* Run `./build` to generate new static HTML.
* Commit and push all changes in `html/` and `src/` to update the tutorial.

## More
* "mkdocs serve" starts a http server listening on http://localhost:8000
  and updates the docs on change.
* Sometimes function names collide. These packages must be loaded _first_
  in "build". That way mlr overwrites these functions again, e.g. caret::train.
* The build caches the output of running the R commands in the cache/ directory.
  If your R setup has changed (e.g. new version of mlr), you should delete
  everything in the cache directory to make sure that the tutorial is
  regenerated with the new code.
