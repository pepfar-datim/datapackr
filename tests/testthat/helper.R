
test_sheet <-
  function(fname)
    testthat::test_path("sheets", fname)
    #rprojroot::find_package_root_file("inst/extdata",fname)

library(httptest)
test_config <- function(fname) rprojroot::find_testthat_root_file("config", fname)

secrets = "~/.secrets/datim.json"
