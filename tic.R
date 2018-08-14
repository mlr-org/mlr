# condition on env variable

if (Sys.getenv("RCMDCHECK") == "TRUE") {

  get_stage("install") %>%
    add_code_step(if (length(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()]) > 0)
      install.packages(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()])) %>%
    add_code_step(devtools::update_packages(TRUE)) %>%
    add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
                                           "-install-package", "thirdparty/XMeans1.0.4.zip"))) %>%
    add_code_step(devtools::install_github("pat-s/rcmdcheck@build-args")) %>% # FIXME: If this is solved in r-lib/rcmdcheck
    add_code_step(devtools::install_deps(upgrade = TRUE, dependencies = TRUE))

  get_stage("before_script") %>%
    add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
                                           "-install-package", "thirdparty/XMeans1.0.4.zip")))

  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

get_stage("script") %>%
    add_code_step(devtools::document()) %>%
    add_step(step_rcmdcheck(notes_are_errors = FALSE, build_args = "--no-build-vignettes",
                            check_args = "--ignore-vignettes --no-manual --as-cran"))
}

if (Sys.getenv("TUTORIAL") == "HTML") {

  get_stage("install") %>%
    add_code_step(if (length(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()]) > 0)
      install.packages(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()])) %>%
    add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
                                           "-install-package", "thirdparty/XMeans1.0.4.zip")))

  get_stage("install") %>%
    add_code_step(if (length(find.package("magick", quiet = TRUE)) == 0) install.packages("magick")) %>% # favicon creation
    add_code_step(if (length(find.package("pander", quiet = TRUE)) == 0) install.packages("pander")) %>%
    add_code_step(devtools::install_deps(upgrade = TRUE, dependencies = TRUE))

  get_stage("after_script") %>%
    add_code_step(devtools::document(roclets=c('rd', 'collate', 'namespace'))) %>%
    add_step(step_build_pkgdown())

  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("deploy") %>%
    add_step(step_push_deploy())
}
