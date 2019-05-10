get_stage("before_script") %>%
  # fix the RWeka warning on Java 9+: http://weka.8497.n7.nabble.com/warnings-td42935.html
  add_code_step(system("export _JAVA_OPTIONS='--add-opens=java.base/java.lang=ALL-UNNAMED'")) %>%
  add_code_step(RWeka::WPM("refresh-cache")) %>%
  add_code_step(RWeka::WPM('install-package', 'XMeans'))

# R CMD Check
do_package_checks(args = "--as-cran", error_on = "error",
  repos = c(getOption("repos"), remotes::bioc_install_repos()))

# pkgdown
if (ci_is_env("FULL", "true")) {
  do_pkgdown(commit_paths = "docs/*", document = FALSE)
}

# only deploy man files in in master branch
if (ci_get_branch() == "master" && ci_is_env("FULL", "true")) {

  get_stage("deploy") %>%
    add_code_step(pkgbuild::compile_dll()) %>%
    add_code_step(devtools::document()) %>%
    add_step(step_push_deploy(commit_paths = c("man/", "DESCRIPTION", "NAMESPACE")))
}
