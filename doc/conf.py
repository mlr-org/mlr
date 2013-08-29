# -*- coding: utf-8 -*-

import sys, os

project = 'mlr'
copyright = '2010-2011, Bernd Bischl et.al.'
version = '1.0.0'
# The full version, including alpha/beta/rc tags.
release = version

extensions = ['sphinx.ext.pngmath', 'sphinx.ext.extlinks']

master_doc = 'index'
templates_path = ['_templates']
source_suffix = '.rst'
pygments_style = 'sphinx' # Source code highlighting style

## HTML settings:
html_theme = '_theme'
html_theme_path = ['.']
html_title = 'mlr - Machine Learning in R'
html_short_title = html_title
html_logo = '_images/mlr_logo-120x64.png'
html_sidebars = {
    'index': ['index_sidebar.html', 'searchbox.html']
}
html_copy_source = False ## Not the R sources, the .rst files.
html_static_path = ['_static']
html_domain_indices = False
html_use_index = False
html_show_sourcelink = False
html_show_sphinx = True
html_show_copyright = True
pngmath_use_preview = True

## LaTeX settings:
latex_paper_size = 'a4paper'
latex_font_size = '11pt'
latex_documents = [
  ('index', 'mlr.tex', u'mlr - Machine Learning in R',
   u'Bernd Bischl et.al.', 'manual'),
]
latex_logo = '_images/mlr_logo.png'
latex_elements = {
    'fontpkg': '\\usepackage{palatino}',
}
latex_show_urls = 'footnote'
