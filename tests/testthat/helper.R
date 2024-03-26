test_sheet <- function(fname) testthat::test_path("sheets", fname)

test_config <- function(fname) {
  rprojroot::find_testthat_root_file("config", fname)
}

onCI <- isTRUE(as.logical(Sys.getenv("CI")))

#' Title
#'
#' @param path
#' @description This helper function eases the process to find files
#' which are needed during testing. Thsese template files however
#' will be located in different places, depending on the testing
#' infrastructure.
#'
#' If we are on the CI server, we know the path, so we will just use that.
#' Otherwise, if the test is run from within Rstudio or the command line
#' the actual location of the template files may be different.
#'
#' The function devtools::test() is also used during covr::report(),
#' so this test route must work as well.
#'
#' @return Full file path to the file
#'
getTemplate <- function(path) {

  if (onCI) {
    return(paste0("/root/project/inst/extdata/", path))
  }


  first <- rprojroot::find_package_root_file("inst/extdata", path)
  second <- rprojroot::find_package_root_file("extdata", path)

  if (file.exists(first)) {
    return(first)
  } else if (file.exists(second)) {
    return(second)
  } else {
    stop("No template could be found")
  }


}

getRDA <- function(object_name) {

  if (onCI) {
    return(paste0("/root/project/data/", object_name, ".rda"))
  }

  first <- rprojroot::find_package_root_file("data", paste0(object_name, ".rda"))
  second <- paste0(getwd(), "/", object_name, ".rda")
  if (file.exists(first)) {
    return(first)
  } else if (file.exists(second)) {
    return(second)
  } else {
    stop("No template could be found")
  }
}

# login object stubs sufficient for use in mocked api calls
# one needed for each server with mock calls
play2361 <- list(base_url = "https://play.dhis2.org/2.36.1/",
                 handle = httr::handle("https://play.dhis2.org/2.36.1/"))

training <- list(base_url = "https://training.datim.org/",
                 handle = httr::handle("https://training.datim.org/"))

triage <- list(base_url = "https://triage.testing.datim.org/",
                 handle = httr::handle("https://triage.testing.datim.org/"))
