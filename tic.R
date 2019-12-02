get_stage("install") %>%
  # avoid build failure if packages are not avail for specific R versions
  add_code_step(Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true"))

get_stage("script") %>%
  add_code_step(RWeka::WPM("refresh-cache")) %>%
  add_code_step(RWeka::WPM('install-package', 'XMeans'))

# R CMD Check
do_package_checks(args = "--as-cran", error_on = "error", codecov = FALSE)

# pkgdown
if (ci_is_env("FULL", "true")) {
  get_stage("before_deploy") %>%
    add_step(step_install_github("mlr-org/mlr3pkgdowntemplate"))
  do_pkgdown()
}

# only deploy man files in in master branch
if (ci_is_env("FULL", "true")) {

  get_stage("deploy") %>%
    add_code_step(pkgbuild::compile_dll()) %>%
    add_code_step(devtools::document()) %>%
    add_step(step_push_deploy(commit_paths = c("man/", "DESCRIPTION", "NAMESPACE")))
}
