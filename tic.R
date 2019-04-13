if (ci_has_env("Check")) {

  get_stage("install") %>%
    #add_step(step_install_cran("stringi")) %>%
    add_step(step_install_cran("digest")) %>%
    add_step(step_install_cran("pander")) %>% # for tutorial
    add_step(step_install_deps(repos = c(getOption("repos"), remotes::bioc_install_repos())))

    get_stage("before_script") %>%
      add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
                                             "-install-package", "thirdparty/XMeans1.0.4.zip")))

    get_stage("script") %>%
      add_step(step_install_deps(repos = c(getOption("repos"), remotes::bioc_install_repos()))) %>%
      add_step(step_rcmdcheck("--as-cran", error_on = "error"))

    get_stage("before_deploy") %>%
      add_step(step_setup_ssh())

    get_stage("deploy") %>%
      add_step(step_build_pkgdown()) %>%
      step_push_deploy(commit_paths = "docs/*")

    # only deploy man files in in master branch
    if (ci_get_branch() == "master") {

      get_stage("deploy") %>%
        add_code_step(devtools::document()) %>%
        add_step(step_push_deploy(commit_paths = c("man/", "DESCRIPTION", "NAMESPACE")))
    }
}
