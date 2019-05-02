get_stage("before_script") %>%
  add_code_step(system2("java", args = c("-cp", "$HOME/R/Library/RWekajars/java/weka.jar weka.core.WekaPackageManager",
    "-install-package", "thirdparty/XMeans1.0.4.zip")))

# R CMD Check
do_package_checks(args = "--as-cran", error_on = "error",
  repos = c(getOption("repos"), remotes::bioc_install_repos()))

# pkgdown
do_pkgdown(commit_paths = "docs/*", document = FALSE)

# only deploy man files in in master branch
if (ci_get_branch() == "master") {

  get_stage("deploy") %>%
    add_code_step(pkgbuild::compile_dll()) %>%
    add_code_step(devtools::document()) %>%
    add_step(step_push_deploy(commit_paths = c("man/", "DESCRIPTION", "NAMESPACE")))
}
