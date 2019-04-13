if (ci_has_env("RCMDCHECK")) {

  get_stage("install") %>%
    add_step(step_install_cran("stringi")) %>%
    add_step(step_install_cran("digest")) %>%
    add_step(step_install_cran("pander")) %>% # for tutorial
    add_code_step(if (length(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()]) > 0) {
      paste0("Installing WARMUPPKGS", trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()])
      install.packages(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()])
    })

    get_stage("before_script") %>%
      add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
                                             "-install-package", "thirdparty/XMeans1.0.4.zip")))

    get_stage("script") %>%
      add_step(step_install_deps(repos = c(getOption("repos"), remotes::bioc_install_repos()))) %>%
      add_step(step_rcmdcheck("--as-cran", error_on = "error"))

  # only deploy in master branch
  if (ci_get_branch() == "master") {

    get_stage("before_deploy") %>%
      add_step(step_setup_ssh())

    get_stage("deploy") %>%
      add_code_step(devtools::document()) %>%
      add_step(step_build_pkgdown()) %>%
      add_step(step_push_deploy(commit_paths = c("man/", "DESCRIPTION", "NAMESPACE", "docs/*")))
  }
}
