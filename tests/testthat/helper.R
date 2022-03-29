library(httptest)
library(tibble)
test_sheet <-
  function(fname)
    testthat::test_path("sheets", fname)
    #rprojroot::find_package_root_file("inst/extdata", fname)

test_config <- function(fname) rprojroot::find_testthat_root_file("config", fname)

# login object stubs sufficient for use in mocked api calls
# one needed for each server with mock calls
play2361 <- list(base_url = "https://play.dhis2.org/2.36.1/",
                 handle = httr::handle("https://play.dhis2.org/2.36.1/"))

training <- list(base_url = "https://training.datim.org/",
                 handle = httr::handle("https://training.datim.org/"))
