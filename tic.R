# remove possible lock files of packages in every stage

get_stage("after_script") %>%
  add_code_step(system("rm -rf $HOME/R/Library/00LOCK-*"))

# condition on env variable

if (Sys.getenv("RCMDCHECK") == "TRUE") {

  get_stage("install") %>%
    add_step(step_install_cran("stringi", type = "both")) %>%
    add_code_step(if (length(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()]) > 0) {
      paste0("Installing WARMUPPKGS", trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()])
      install.packages(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()])
    }
    ) %>%
    add_code_step(devtools::update_packages(TRUE))

  if (inherits(ci(), "TravisCI")) {
    get_stage("before_script") %>%
      add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
                                             "-install-package", "thirdparty/XMeans1.0.4.zip")))
  }

  get_stage("script") %>%
    add_code_step(devtools::document()) %>%
    # manual approch until https://github.com/r-lib/rcmdcheck/issues/83#issuecomment-424314978 is solved
    add_code_step(devtools::build(manual = TRUE)) %>%
    add_step(step_rcmdcheck(path = "../mlr_2.13.9000.tar.gz", args = "--as-cran",
                            error_on = "error"))

  if (!Sys.getenv("TRAVIS_EVENT_TYPE") == "cron") {

    get_stage("before_deploy") %>%
      add_step(step_setup_ssh())

    get_stage("deploy") %>%
      add_code_step(devtools::document()) %>%
      add_step(step_push_deploy(commit_paths = "man/"))
  }
}

if (Sys.getenv("TUTORIAL") == "HTML") {

  get_stage("install") %>%
    add_code_step(if (length(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()]) > 0)
      install.packages(trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]])[!trimws(strsplit(Sys.getenv("WARMUPPKGS"), " ")[[1]]) %in% installed.packages()])) %>%
    add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
                                           "-install-package", "thirdparty/XMeans1.0.4.zip")))

  get_stage("install") %>%
    add_step(step_install_cran("magick")) %>% # favicon creation
    add_step(step_install_cran("pander"))

  if (!Sys.getenv("TRAVIS_EVENT_TYPE") == "cron") {

    get_stage("before_deploy") %>%
      add_step(step_setup_ssh())

    get_stage("deploy") %>%
      add_step(step_build_pkgdown()) %>%
      add_step(step_push_deploy(commit_paths = "docs/*"))

  }
}
