# condition on env variable
# if (Sys.getenv("check") == "TRUE") {
#
#   get_stage("install") %>%
#     add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
#                                            "-install-package", "thirdparty/XMeans1.0.4.zip"))) %>%
#     add_code_step(devtools::document())
#
#   get_stage("script") {
#     add_code_step(system2("travis_wait"
#   }
#
# }

if (Sys.getenv("TUTORIAL") == "HTML") {

  get_stage("install") %>%
    add_code_step(if (length(find.package("magick", quiet = TRUE)) == 0) install.packages("magick")) %>% # favicon creation
    add_code_step(devtools::install_deps(upgrade = TRUE, dependencies = TRUE))

  get_stage("before_deploy") %>%
    add_step(step_setup_ssh()) #%>%
    # this pkgdown fork includes the tweaked navbar for mlr
    #add_code_step(devtools::install_github("pat-s/pkgdown"))

  get_stage("deploy") %>%
    add_step(step_build_pkgdown()) %>%
    add_step(step_push_deploy(orphan = TRUE, path = "docs", branch = "gh-pages"))

} else if (Sys.getenv("TUTORIAL") == "PDF") {

  get_stage("install") %>%
    add_code_step(if (length(find.package("pander", quiet = TRUE)) == 0) install.packages("pander")) %>%
    add_code_step(if (length(find.package("fs", quiet = TRUE)) == 0) install.packages("fs")) %>%
    add_code_step(if (length(find.package("rmarkdown", quiet = TRUE)) == 0) install.packages("rmarkdown")) %>%
    add_code_step(if (length(find.package("bookdown", quiet = TRUE)) == 0) install.packages("bookdown")) %>%
    add_code_step(if (length(find.package("roxygen2", quiet = TRUE)) == 0) devtools::install_github("klutometis/roxygen")) %>%
    add_code_step(devtools::install_deps(upgrade = TRUE, dependencies = TRUE))

  # this ensures that the NAMESPACE is correct. R CMD Build is not enough for the PDF build.
  get_stage("script") %>%
    add_code_step(devtools::document())

  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("deploy") %>%
    add_code_step(rmarkdown::render("vignettes/tutorial/devel/pdf/_pdf_wrapper.Rmd")) %>%
    add_code_step(fs::file_move("vignettes/tutorial/devel/pdf/_pdf_wrapper.pdf", "vignettes/tutorial/devel/pdf/mlr-tutorial.pdf")) %>%
    add_step(step_push_deploy(orphan = FALSE, path = "vignettes/tutorial/devel/pdf", branch = "master",
                              remote_url = "git@github.com:pat-s/mlr.git"))
}

