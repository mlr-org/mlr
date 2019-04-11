# remove possible lock files of packages in every stage

get_stage("after_script") %>%
  add_code_step(system("rm -rf $HOME/R/Library/00LOCK-*"))

# condition on env variable

if (Sys.getenv("RCMDCHECK") == "TRUE") {

  get_stage("install") %>%
    add_step(step_install_cran("stringi")) %>%
    add_step(step_install_cran("digest")) %>%
    add_code_step(if (length(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()]) > 0) {
      paste0("Installing WARMUPPKGS", trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()])
      install.packages(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()])
    }
    ) %>%
    add_code_step(remotes::update_packages(TRUE))

  if (inherits(ci(), "TravisCI")) {
    get_stage("before_script") %>%
      add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
                                             "-install-package", "thirdparty/XMeans1.0.4.zip")))
  }

  if (inherits(ci(), "TravisCI")) {

    get_stage("script") %>%
      add_code_step(pkgbuild::compile_dll()) %>%
      add_code_step(devtools::document()) %>%
      add_step(step_rcmdcheck("--as-cran", error_on = "error"))
  }

  # only deploy in master branch
  if (ci()$get_branch() == "master") {

    get_stage("before_deploy") %>%
      add_step(step_setup_ssh())

    get_stage("deploy") %>%
      add_code_step(pkgbuild::compile_dll()) %>%
      add_code_step(devtools::document()) %>%
      add_step(step_push_deploy(commit_paths = c("man/", "DESCRIPTION", "NAMESPACE")))
  }
}

if (Sys.getenv("TUTORIAL") == "HTML") {

  get_stage("install") %>%
    add_code_step(if (length(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()]) > 0)
      install.packages(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()])) %>%
    add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
                                           "-install-package", "thirdparty/XMeans1.0.4.zip")))

  get_stage("install") %>%
    add_step(step_install_cran("pander"))

    get_stage("before_deploy") %>%
      add_step(step_setup_ssh())

    get_stage("deploy") %>%
      add_step(step_build_pkgdown(document = FALSE)) %>%
      add_step(step_push_deploy(commit_paths = "docs/*"))
}
