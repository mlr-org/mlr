# condition on env variable

if (Sys.getenv("RCMDCHECK") == "TRUE") {

  get_stage("install") %>%
    add_code_step(if (length(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()]) > 0)
      install.packages(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()])) %>%
    add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
                                           "-install-package", "thirdparty/XMeans1.0.4.zip"))) %>%
    add_code_step(devtools::install_deps(upgrade = TRUE, dependencies = TRUE))

  get_stage("before_script") %>%
    add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
                                           "-install-package", "thirdparty/XMeans1.0.4.zip")))

  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("script") %>%
    add_code_step(devtools::document()) %>%
    add_step(step_rcmdcheck())

  get_stage("deploy") %>%
    add_code_step(system2("bash", args = c("inst/convert_to_ascii_news.sh"))) %>%
    add_step(step_push_deploy(orphan = FALSE, branch = "master", commit_paths = c("NAMESPACE", "man/*", "NEWS")))
}

if (Sys.getenv("TUTORIAL") == "HTML") {

  get_stage("install") %>%
    add_code_step(if (length(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()]) > 0)
      install.packages(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()])) %>%
    add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
                                           "-install-package", "thirdparty/XMeans1.0.4.zip"))) %>%
    add_code_step(devtools::install_deps(upgrade = TRUE, dependencies = TRUE))

  get_stage("install") %>%
    add_code_step(if (length(find.package("magick", quiet = TRUE)) == 0) install.packages("magick")) %>% # favicon creation
    add_code_step(if (length(find.package("pander", quiet = TRUE)) == 0) install.packages("pander")) %>%
    add_code_step(devtools::install_deps(upgrade = TRUE, dependencies = TRUE))

  get_stage("before_deploy") %>%
    add_step(step_setup_ssh()) %>%
    add_code_step(system2("sed", c("-i","-e", '/^##/ s/#/', "-e", "'/^###/ s/#/'", "'/^####/ s/#/'", "vignettes/tutorial/devel/*.Rmd"))) %>%
    add_code_step(system2("sed", c("-i","-e", '/^##/ s/#/', "-e", "'/^###/ s/#/'", "'/^####/ s/#/'", "vignettes/tutorial/release/*.Rmd")))

  get_stage("deploy") %>%
    add_step(step_build_pkgdown(lazy = TRUE)) %>%
    add_step(step_push_deploy(orphan = TRUE, path = "docs", branch = "gh-pages"))

} else if (Sys.getenv("TUTORIAL") == "PDFdev") {

  get_stage("install") %>%
    add_code_step(if (length(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()]) > 0)
      install.packages(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()])) %>%
    add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
                                           "-install-package", "thirdparty/XMeans1.0.4.zip"))) %>%
    add_code_step(devtools::install_deps(upgrade = TRUE, dependencies = TRUE))

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
    add_code_step(fs::file_move("vignettes/tutorial/devel/pdf/_pdf_wrapper.pdf", "vignettes/tutorial/devel/pdf/mlr-tutorial_dev.pdf")) %>%
    add_step(step_push_deploy(orphan = FALSE, commit_paths = "vignettes/tutorial/dev/pdf/mlr-tutorial_dev.pdf", branch = "tutorial_pdf"))
}

if (Sys.getenv("TUTORIAL") == "PDFrelease") {

  get_stage("install") %>%
    add_code_step(if (length(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()]) > 0)
      install.packages(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()])) %>%
    add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
                                           "-install-package", "thirdparty/XMeans1.0.4.zip"))) %>%
    add_code_step(devtools::install_deps(upgrade = TRUE, dependencies = TRUE))

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
    add_code_step(rmarkdown::render("vignettes/tutorial/release/pdf/_pdf_wrapper.Rmd")) %>%
    add_code_step(fs::file_move("vignettes/tutorial/release/pdf/_pdf_wrapper.pdf", "vignettes/tutorial/release/pdf/mlr-tutorial_release.pdf")) %>%
    add_step(step_push_deploy(orphan = FALSE, commit_paths = "vignettes/tutorial/release/pdf/mlr-tutorial_release.pdf", branch = "tutorial_pdf"))
}
