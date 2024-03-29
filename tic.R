if (Sys.info()[["sysname"]] != "Windows") {
  get_stage("script") %>%
    add_code_step(RWeka::WPM("refresh-cache")) %>%
    add_code_step(RWeka::WPM("install-package", "XMeans"))
}

# R CMD Check
do_package_checks(error_on = "warning", codecov = FALSE)

# pkgdown
if (ci_on_ghactions() && ci_has_env("BUILD_PKGDOWN")) {
  get_stage("before_deploy") %>%
    add_step(step_install_github("mlr-org/mlr3pkgdowntemplate")) %>%
    add_step(step_install_cran("GGally"))
  do_pkgdown()
}

if (ci_is_env("CODECOV", "true")) {
  get_stage("after_success") %>%
    add_code_step(RWeka::WPM("refresh-cache")) %>%
    add_code_step(RWeka::WPM("install-package", "XMeans")) %>%
    add_code_step(covr::codecov())
}
