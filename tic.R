#add_package_checks()

# condition on env variable
if (Sys.getenv("TUTORIAL") == "HTML") {
  # pkgdown documentation can be built optionally. Other example criteria:
  # - `inherits(ci(), "TravisCI")`: Only for Travis CI
  # - `ci()$is_tag()`: Only for tags, not for branches
  # - `Sys.getenv("BUILD_PKGDOWN") != ""`: If the env var "BUILD_PKGDOWN" is set
  # - `Sys.getenv("TRAVIS_EVENT_TYPE") == "cron"`: Only for Travis cron jobs

  get_stage("install") %>%
    add_code_step(if (length(find.package("pander", quiet = TRUE)) == 0) install.packages("pander")) %>%
    add_code_step(if (length(find.package("rmarkdown", quiet = TRUE)) == 0) install.packages("rmarkdown"))# %>%
    add_code_step(devtools::install_deps(upgrade = TRUE, dependencies = TRUE))

  get_stage("script") %>%
    add_code_step(devtools::document())

  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("deploy") %>%
    add_step(step_build_pkgdown()) %>%
    add_step(step_push_deploy(orphan = TRUE, path = "docs", branch = "gh-pages"))
} else if (Sys.getenv("TUTORIAL") == "PDF") {

  get_stage("install") %>%
    add_code_step(if (length(find.package("magick", quiet = TRUE)) == 0) install.packages("magick")) %>%
    add_code_step(if (length(find.package("pander", quiet = TRUE)) == 0) install.packages("pander")) %>%
    add_code_step(if (length(find.package("fs", quiet = TRUE)) == 0) install.packages("fs")) %>%
    add_code_step(if (length(find.package("rmarkdown", quiet = TRUE)) == 0) install.packages("rmarkdown")) #%>%
    add_code_step(devtools::install_deps(upgrade = TRUE, dependencies = TRUE))

  get_stage("script") %>%
    add_code_step(devtools::document())

  get_stage("before_deploy") %>%
    add_step(step_setup_ssh()) %>%
    add_code_step(devtools::install_github("jimhester/lintr")) %>%
    add_code_step(devtools::install_github("pat-s/pkgdown@cc1579abcf00cb11bc856e48f3b9d3c91432c2c2"))

  get_stage("deploy") %>%
    add_code_step(rmarkdown::render("vignettes/tutorial/devel/pdf/_pdf_wrapper.Rmd")) %>%
    add_code_step(fs::file_move("vignettes/tutorial/devel/pdf/_pdf_wrapper.pdf", "vignettes/tutorial/devel/pdf/mlr-tutorial.pdf")) %>%
    add_step(step_push_deploy(orphan = FALSE, path = "vignettes/tutorial/devel/pdf/mlr-tutorial.pdf", branch = "master"))
}

